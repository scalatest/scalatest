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

class ListShouldContainOneOfLogicalOrSpec extends Spec {
  
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
  
  val fileName: String = "ListShouldContainOneOfLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain oneOf (..) or contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (contain oneOf ("fee", "fie", "foe", "fam") or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (contain oneOf ("fee", "fie", "foe", "fum") or contain oneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fam") or contain oneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fie", "fee", "fum", "foe"))
        fumList should (contain oneOf ("fie", "fee", "fum", "foe") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") or (contain oneOf ("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain oneOf ("fie", "fee", "fum", "foe") or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("fie", "fee", "fum", "foe") or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf ("fee", "fie", "foe", "fie", "fum") or contain oneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") or contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (equal (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain oneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (fumList) or contain oneOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain oneOf ("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (be (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (be (fumList) or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (be (toList) or contain oneOf("fie", "fee", "fum", "foe"))
        fumList should (be (fumList) or contain oneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (be (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (be (fumList) or contain oneOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain oneOf ("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (be (fumList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain oneOf ("fie", "fee", "fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }

    object `when used with (contain oneOf (..) and be (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain oneOf("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain oneOf("FEE", "FIE", "FOE", "FUM") or be (fumList))
        fumList should (contain oneOf("fie", "fee", "fum", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or be (fumList))
        fumList should (contain oneOf ("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain oneOf ("fie", "fee", "fum", "foe") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain oneOf ("fie", "fee", "fum", "foe") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain oneOf ("FEE", "FIE", "FOE", "FUM") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain oneOf ("fie", "fee", "fum", "foe") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain oneOf("fee", "fie", "foe", "fie", "fum") or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("fee", "fie", "foe", "fum"))
        fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain oneOf ("fee", "fie", "foe", "fum") or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (contain oneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain oneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain oneOf ("fee", "fie", "foe", "fie", "fum") or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain oneOf ("FEE", "FIE", "FOE", "FUM") or not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not equal (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (fumList) or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not equal (toList) or not contain oneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain oneOf ("fie", "fee", "fum", "foe"))
        fumList should (not equal (fumList) or not contain oneOf ("fie", "fee", "fum", "foe"))
        fumList should (not equal (toList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain oneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not contain oneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain oneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not be (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (fumList) or not contain oneOf("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (toList) or not contain oneOf("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain oneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain oneOf ("fee", "fie", "foe", "fum"))
        fumList should (not be (fumList) or not contain oneOf ("fee", "fie", "foe", "fum"))
        fumList should (not be (toList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain oneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain oneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain oneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain oneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain oneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
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
    
    object `when used with (contain oneOf (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (contain oneOf (3, 2, 1) or contain oneOf (1, 3, 4))
        all (list1s) should (contain oneOf (3, 2, 5) or contain oneOf (1, 3, 4))
        all (list1s) should (contain oneOf (3, 2, 1) or contain oneOf (2, 3, 4))
        
        atLeast (2, lists) should (contain oneOf (3, 1, 5) or contain oneOf (1, 3, 4))
        atLeast (2, lists) should (contain oneOf (3, 6, 5) or contain oneOf (1, 3, 4))
        atLeast (2, lists) should (contain oneOf (3, 1, 5) or contain oneOf (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain oneOf (6, 7, 8) or contain oneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " did not contain one of (6, 7, 8), and " + decorateToStringValue(List(2)) + " did not contain one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain oneOf ("HI", "HE") or contain oneOf ("HO", "HI"))
        all (hiLists) should (contain oneOf ("hi", "he") or contain oneOf ("HO", "HI"))
        all (hiLists) should (contain oneOf ("HI", "HE") or contain oneOf ("ho", "hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain oneOf ("hi", "he") or contain oneOf ("ho", "hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain one of (\"hi\", \"he\"), and " + decorateToStringValue(List("hi")) + " did not contain one of (\"ho\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain oneOf ("HI", "HE") or contain oneOf ("HO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain oneOf ("hi", "he") or contain oneOf ("HO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain oneOf ("HI", "HE") or contain oneOf ("ho", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain oneOf ("hi", "he") or contain oneOf ("ho", "hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain one of (\"hi\", \"he\"), and " + decorateToStringValue(List("hi")) + " did not contain one of (\"ho\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain oneOf (3, 2, 2, 1) or contain oneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain oneOf (1, 3, 4) or contain oneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (be (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1)) or contain oneOf (1, 3, 4))
        all (list1s) should (be (List(2)) or contain oneOf (1, 3, 4))
        all (list1s) should (be (List(1)) or contain oneOf (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2)) or contain oneOf (2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was not equal to " + decorateToStringValue(List(2)) + ", and " + decorateToStringValue(List(1)) + " did not contain one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi")) or contain oneOf ("HI", "HE"))
        all (hiLists) should (be (List("ho")) or contain oneOf ("HI", "HE"))
        all (hiLists) should (be (List("hi")) or contain oneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) or contain oneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi")) or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho")) or contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi")) or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) or contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(1)) or contain oneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain oneOf (3, 2, 8) or not contain oneOf (8, 3, 4))
        all (list1s) should (not contain oneOf (1, 2, 8) or not contain oneOf (8, 3, 4))
        all (list1s) should (not contain oneOf (3, 2, 8) or not contain oneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain oneOf (2, 6, 8) or not contain oneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " contained one of (2, 6, 8), and " + decorateToStringValue(List(2)) + " contained one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain oneOf ("hi", "he") or not contain oneOf ("hi", "he"))
        all (hiLists) should (not contain oneOf ("HI", "HE") or not contain oneOf ("hi", "he"))
        all (hiLists) should (not contain oneOf ("hi", "he") or not contain oneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain oneOf ("HI", "HE") or not contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\"), and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain oneOf ("hi", "he") or not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain oneOf ("HI", "HE") or not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain oneOf ("hi", "he") or not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain oneOf ("HI", "HE") or not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\"), and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain oneOf (3, 2, 2, 1) or not contain oneOf (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain oneOf (8, 3, 4) or not contain oneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
    
    object `when used with (not be (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not contain oneOf (8, 3, 4))
        all (list1s) should (not be (List(1)) or not contain oneOf (8, 3, 4))
        all (list1s) should (not be (List(2)) or not contain oneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(1)) or not contain oneOf (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", and " + decorateToStringValue(List(1)) + " contained one of (2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) or not contain oneOf ("hi", "he"))
        all (hiLists) should (not be (List("hi")) or not contain oneOf ("hi", "he"))
        all (hiLists) should (not be (List("ho")) or not contain oneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) or not contain oneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) or not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hi")) or not contain oneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("ho")) or not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) or not contain oneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value` {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) or not contain oneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("oneOfDuplicate")))
      }
    }
  }
}
