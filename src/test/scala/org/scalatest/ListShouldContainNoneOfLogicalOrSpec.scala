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

import org.scalactic.Equality
import org.scalactic.Uniformity
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import scala.collection.JavaConverters._
import Matchers._

class ListShouldContainNoneOfLogicalOrSpec extends Spec {
  
  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val invertedListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
    }
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b
    }
  
  val upperCaseListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a.map(_.toUpperCase) == b
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
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainNoneOfLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain noneOf (..) or contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain noneOf ("fee", "fie", "foe", "fam") or contain noneOf("fie", "fee", "fam", "foe"))
        fumList should (contain noneOf ("fee", "fie", "foe", "fam") or contain noneOf("fie", "fee", "fum", "foe"))
        fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf ("fee", "fie", "fum", "foe"))
        fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or contain noneOf ("fee", "fie", "fum", "foe"))
        fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf ("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or (contain noneOf ("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or contain noneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain noneOf ("fee", "fie", "foe", "fum") or contain noneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or contain noneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FAM ") or contain noneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain noneOf ("fee", "fie", "foe", "fie", "fum") or contain noneOf("fie", "fee", "fam", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain noneOf ("fie", "fee", "fam", "foe") or contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (equal (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain noneOf("fie", "fee", "fam", "foe"))
        fumList should (equal (toList) or contain noneOf("fie", "fee", "fam", "foe"))
        fumList should (equal (fumList) or contain noneOf("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain noneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain noneOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain noneOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain noneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain noneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain noneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain noneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (be (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be_== (fumList) or contain noneOf("fie", "fee", "fam", "foe"))
        fumList should (be_== (toList) or contain noneOf("fie", "fee", "fam", "foe"))
        fumList should (be_== (fumList) or contain noneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or contain noneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be_== (fumList) or contain noneOf ("fee", "fie", "foe", "fum"))
        fumList should (be_== (toList) or contain noneOf ("fee", "fie", "foe", "fum"))
        fumList should (be_== (fumList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be_== (toList) or (contain noneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (be_== (fumList) or contain noneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (toList) or contain noneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (be_== (fumList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be_== (toList) or contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (be_== (fumList) or contain noneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be_== (fumList) or contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }

    object `when used with (contain noneOf (..) and be (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain noneOf("fie", "fee", "fam", "foe") or be_== (fumList))
        fumList should (contain noneOf("fie", "fee", "fum", "foe") or be_== (fumList))
        fumList should (contain noneOf("fie", "fee", "fam", "foe") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("fie", "fee", "fum", "foe") or be_== (toList))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain noneOf ("fee", "fie", "foe", "fum") or be_== (fumList))
        fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or be_== (fumList))
        fumList should (contain noneOf ("fee", "fie", "foe", "fum") or be_== (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or be_== (toList))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain noneOf ("fee", "fie", "foe", "fum") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or be_== (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain noneOf ("fee", "fie", "foe", "fum") or be_== (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain noneOf ("FEE", "FIE", "FOE", "FUM") or be_== (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ") or be_== (fumList))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain noneOf("fee", "fie", "foe", "fie", "fum") or be_== (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (not contain noneOf (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf("fee", "fie", "fum", "foe"))
        fumList should (not contain noneOf ("fee", "fie", "foe", "fam") or not contain noneOf("fee", "fie", "fum", "foe"))
        fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf("fee", "fie", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noneOf ("fee", "fie", "foe", "fam") or not contain noneOf ("fee", "fie", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain noneOf ("FEE", "FIE", "FOE", "FUM") or not contain noneOf ("FEE", "FIE", "FUM", "FOE"))
        fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf ("FEE", "FIE", "FUM", "FOE"))
        fumList should (not contain noneOf ("FEE", "FIE", "FOE", "FUM") or not contain noneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not contain noneOf ("FEE", "FIE", "FOE", "FUM") or not contain noneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain noneOf ("FEE", "FIE", "FOE", "FUM") or not contain noneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain noneOf ("fee", "fie", "foe", "fum") or not contain noneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain noneOf ("fee", "fie", "foe", "fie", "fum") or not contain noneOf("fee", "fie", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain noneOf ("fee", "fie", "fum", "foe") or not contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (not equal (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain noneOf("fee", "fie", "foe", "fum"))
        fumList should (not equal (fumList) or not contain noneOf("fee", "fie", "foe", "fum"))
        fumList should (not equal (toList) or not contain noneOf("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain noneOf ("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain noneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (not equal (fumList) or not contain noneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (not equal (toList) or not contain noneOf ("fie", "fee", "fum", "foe"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain noneOf ("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain noneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain noneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain noneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain noneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ") or not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (not be (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be_== (toList) or not contain noneOf("fee", "fie", "foe", "fum"))
        fumList should (not be_== (fumList) or not contain noneOf("fee", "fie", "foe", "fum"))
        fumList should (not be_== (toList) or not contain noneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or not contain noneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be_== (toList) or not contain noneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be_== (fumList) or not contain noneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be_== (toList) or not contain noneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be_== (fumList) or (not contain noneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be_== (toList) or not contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (fumList) or not contain noneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be_== (toList) or not contain noneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be_== (fumList) or not contain noneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM ") or not contain noneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be_== (toList) or not contain noneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
  }
  
  object `collection of Lists` {
    
    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(left)
    
    object `when used with (contain noneOf (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain noneOf (3, 6, 9) or contain noneOf (2, 6, 8))
        all (list1s) should (contain noneOf (1, 2, 3) or contain noneOf (2, 6, 8))
        all (list1s) should (contain noneOf (3, 6, 9) or contain noneOf (1, 2, 3))
        
        atLeast (2, lists) should (contain noneOf (2, 6, 8) or contain noneOf (3, 6, 9))
        atLeast (2, lists) should (contain noneOf (1, 2, 3) or contain noneOf (3, 6, 9))
        atLeast (2, lists) should (contain noneOf (2, 6, 8) or contain noneOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain noneOf (2, 6, 8) or contain noneOf (2, 3, 5))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages("containedAtLeastOneOf", lists(2), UnquotedString("2, 6, 8")) + ", and " + FailureMessages("containedAtLeastOneOf", lists(2), UnquotedString("2, 3, 5")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain noneOf ("hi", "he") or contain noneOf ("hi", "he"))
        all (hiLists) should (contain noneOf ("hi", "he") or contain noneOf ("HI", "HE"))
        all (hiLists) should (contain noneOf ("HI", "HE") or contain noneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain noneOf ("HI", "HE") or contain noneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")) + ", and " + FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain noneOf ("hi", "he") or contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain noneOf ("hi", "he") or contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain noneOf ("HI", "HE") or contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain noneOf ("HI", "HE") or contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")) + ", and " + FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain noneOf (1, 2, 2, 3) or contain noneOf (2, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain noneOf (2, 6, 8) or contain noneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (be (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be_== (List(1)) or contain noneOf (2, 6, 8))
        all (list1s) should (be_== (List(2)) or contain noneOf (2, 6, 8))
        all (list1s) should (be_== (List(1)) or contain noneOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be_== (List(2)) or contain noneOf (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was not equal to " + decorateToStringValue(List(2)) + ", and " + FailureMessages("containedAtLeastOneOf", list1s(0), UnquotedString("1, 2, 3")), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be_== (List("hi")) or contain noneOf ("hi", "he"))
        all (hiLists) should (be_== (List("ho")) or contain noneOf ("hi", "he"))
        all (hiLists) should (be_== (List("hi")) or contain noneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be_== (List("ho")) or contain noneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be_== (List("hi")) or contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("ho")) or contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be_== (List("hi")) or contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be_== (List("ho")) or contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + FailureMessages("containedAtLeastOneOf", hiLists(0), UnquotedString("\"HI\", \"HE\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be_== (List(1)) or contain noneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (not contain noneOf (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain noneOf (1, 2, 3) or not contain noneOf (1, 6, 8))
        all (list1s) should (not contain noneOf (2, 6, 8) or not contain noneOf (1, 6, 8))
        all (list1s) should (not contain noneOf (1, 2, 3) or not contain noneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain noneOf (1, 6, 8) or not contain noneOf (1, 3, 5))
        }
        checkMessageStackDepth(e1, allErrMsg(2, FailureMessages("didNotContainAtLeastOneOf", lists(2), UnquotedString("1, 6, 8")) + ", and " + FailureMessages("didNotContainAtLeastOneOf", lists(2), UnquotedString("1, 3, 5")), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain noneOf ("HI", "HE") or not contain noneOf ("HI", "HE"))
        all (hiLists) should (not contain noneOf ("hi", "he") or not contain noneOf ("HI", "HE"))
        all (hiLists) should (not contain noneOf ("HI", "HE") or not contain noneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain noneOf ("hi", "he") or not contain noneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")) + ", and " + FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain noneOf ("HI", "HE") or not contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain noneOf ("hi", "he") or not contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain noneOf ("HI", "HE") or not contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain noneOf ("hi", "he") or not contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")) + ", and " + FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain noneOf (1, 2, 2, 3) or not contain noneOf (1, 6, 8))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain noneOf (1, 6, 8) or not contain noneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
    
    object `when used with (not be (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be_== (List(2)) or not contain noneOf (1, 6, 8))
        all (list1s) should (not be_== (List(1)) or not contain noneOf (1, 6, 8))
        all (list1s) should (not be_== (List(2)) or not contain noneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be_== (List(1)) or not contain noneOf (2, 6, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", and " + FailureMessages("didNotContainAtLeastOneOf", list1s(0), UnquotedString("2, 6, 8")), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be_== (List("ho")) or not contain noneOf ("HI", "HE"))
        all (hiLists) should (not be_== (List("hi")) or not contain noneOf ("HI", "HE"))
        all (hiLists) should (not be_== (List("ho")) or not contain noneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be_== (List("hi")) or not contain noneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be_== (List("ho")) or not contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("hi")) or not contain noneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be_== (List("ho")) or not contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be_== (List("hi")) or not contain noneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + FailureMessages("didNotContainAtLeastOneOf", hiLists(0), UnquotedString("\"hi\", \"he\"")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be_== (List(2)) or not contain noneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("noneOfDuplicate")))
      }
    }
  }
}
