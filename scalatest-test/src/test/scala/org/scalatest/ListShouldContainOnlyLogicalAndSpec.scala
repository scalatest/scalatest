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
import Matchers._
import exceptions.TestFailedException

class ListShouldContainOnlyLogicalAndSpec extends FunSpec {
  
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
      case e: java.util.Map.Entry[_, _] => 
        (e.getKey, e.getValue) match {
          case (k: String, v: String) => Entry(k.toUpperCase, v.toUpperCase)
          case _ => value
        }
      case _ => value
    }
  
  val upperCaseListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = upperCase(a) == upperCase(b)
    }
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainOnlyLogicalAndSpec.scala"
  
  describe("a List") {
    
    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")
    
    describe("when used with (contain only (..) and contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain only ("fee", "fie", "foe", "fum") and contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("happy", "birthday", "to", "you") and contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fum") and contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FEE", "FIE", "FOE", "FUM") and contain only ("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FAM") and contain only ("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain only ("FEE", "FIE", "FOE", "FUM") and (contain only ("FEE", "FIE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") and contain only ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FAM") and contain only ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain only ("FEE", "FIE", "FOE", "FUM") and contain only ("FEE", "FIE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") and contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only () and contain only ("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fie", "fee", "fum", "foe") and contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") and contain only ("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fie", "fee", "fum", "foe") and contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Vector("happy", "birthday", "to", "you") and contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fum") and contain only Vector("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (equal (..) and contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain only ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain only ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain only ("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain only ("FEE", "FIE", "FOE", "FAM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain only Vector("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (be (..) and contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain only ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain only ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain only ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain only ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain only ("happy", "birthday", "to", "you"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain only ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain only Vector("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }

    describe("when used with (contain only (..) and be (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain only ("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain only ("happy", "birthday", "to", "you") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain only ("FIE", "FEE", "FUM", "FOE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain only ("FIE", "FEE", "FAM", "FOE") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain only ("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"HAPPY\", \"BIRTHDAY\", \"TO\", \"YOU\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain only ("FIE", "FEE", "FAM", "FOE") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain only ("FIE", "FEE", "FUM", "FOE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only () and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain only ("fee", "fie", "foe", "fie", "fum") and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          fumList should (contain only Vector("happy", "birthday", "to", "you") and be (fumList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(fumList), decorateToStringValue(Vector("happy", "birthday", "to", "you"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not contain only xx and not contain only xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain only ("fee", "fie", "foe", "fuu") and not contain only ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fum") and not contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain only ("happy", "birthday", "to", "you") and not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain only ("FIE", "FEE", "FAM", "FOE") and not contain only ("FIE", "FEE", "FOE", "FAM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain only ("FIE", "FEE", "FUM", "FOE") and not contain only ("FIE", "FEE", "FOE", "FAM"))
        }
        checkMessageStackDepth(e1, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain only ("FIE", "FEE", "FAM", "FOE") and (not contain only ("FIE", "FEE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\"") + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not contain only ("FIE", "FEE", "FAM", "FOE") and not contain only ("FIE", "FEE", "FOE", "FAM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain only ("FIE", "FEE", "FAM", "FOE") and not contain only ("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\"") + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain only ("FIE", "FEE", "FUM", "FOE") and not contain only ("FIE", "FEE", "FOE", "FAM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain only (" FEE ", " FIE ", " FOE ", " FUM ") and contain only (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only () and not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") and not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fee", "fie", "foe", "fie", "fum") and not contain only ("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain only ("fie", "fee", "fuu", "foe") and not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not contain only (Vector("fee", "fie", "foe", "fum")) and not contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not contain only (Vector("happy", "birthday", "to", "you")) and not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector("happy", "birthday", "to", "you"))) + ", but " + Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not equal (..) and not contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain only ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain only ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain only ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") and not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not equal (toList) and not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not be (..) and not contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain only ("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain only ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain only ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain only ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain only ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain only ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain only ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElements(decorateToStringValue(fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain only ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain only (" FEE ", " FIE ", " FOE ", " FUU ") and not contain only (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain only ("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          Vector(Vector("fee", "fie", "foe", "fum")) should (not be (toList) and not contain only (Vector("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(toList)) + ", but " + Resources.containedOnlyElementsWithFriendlyReminder(decorateToStringValue(Vector(Vector("fee", "fie", "foe", "fum"))), decorateToStringValue(Vector("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
    }
    
  }
  
  describe("col of Lists") {
    
    val list1s: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(3, 2, 1))
    val lists: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), List(4, 3, 2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(3, 2, 1), List(3, 2, 1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "hello"), List("hi", "hello"), List("hi", "hello"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(left)
    
    describe("used with contain only xx and contain only xx") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain only (3, 2, 1) and contain only (1, 3, 2))
        atLeast (2, lists) should (contain only (3, 1, 2) and contain only (2, 3, 1))
        atMost (2, lists) should (contain only (3, 1, 2) and contain only (2, 3, 1))
        no (lists) should (contain only (3, 6, 9) and contain only (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain only (1, 2, 3) and contain only (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(1, 2, 3)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain only (1, 2, 3) and contain only (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " contained only " + "(1, 2, 3)" + ", but " + decorateToStringValue(List(3, 2, 1)) + " did not contain only " + "(1, 3, 4)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain only ("hi", "hello") and contain only ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hi\", \"hello\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain only ("HELLO", "HI") and contain only ("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain only ("HO", "HELLO") and contain only ("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain only ("HELLO", "HI") and contain only ("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain only ("HELLO", "HI") and contain only ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain only ("HO", "HELLO") and contain only ("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain only ("HELLO", "HI") and contain only ("HO", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only () and contain only (1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 3, 2) and contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (3, 2, 2, 1) and contain only (1, 3, 2))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain only (1, 3, 2) and contain only (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2)))) should (contain only Vector(1, 2, 3) and contain only (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2))) + " did not contain only " + "(" + decorateToStringValue(Vector(1, 2, 3)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1), Vector(3, 2, 1), Vector(4, 3, 2)))), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(1, 2, 3)))) should (contain only Vector(1, 2, 3) and contain only Vector(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Vector(Vector(1, 2, 3))) + " contained only (" + decorateToStringValue(Vector(1, 2, 3)) + "), did you forget to say : _*, but " + decorateToStringValue(Vector(Vector(1, 2, 3))) + " did not contain only " + "(" + decorateToStringValue(Vector(1, 3, 4)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(1, 2, 3)))), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (contain only Vector("hi", "hello") and contain only Vector("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " contained only (" + decorateToStringValue(Vector("hi", "hello")) + "), did you forget to say : _*, but " + decorateToStringValue(Vector(Vector("hi", "hello"))) + " did not contain only " + "(" + decorateToStringValue(Vector("ho", "hey", "howdy")) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (be (..) and contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (List(3, 2, 1)) and contain only (1, 3, 2))
        atLeast (2, lists) should (be (List(3, 2, 1)) and contain only (1, 3, 2))
        atMost (2, lists) should (be (List(3, 2, 1)) and contain only (2, 3, 1))
        no (lists) should (be (List(3, 6, 9)) and contain only (3, 4, 5))
        no (nils) should (be (List(1, 6, 8)) and contain only (1, 3, 4))
        no (listsNil) should (be (List(2, 6, 8)) and contain only (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(3, 2, 1)) and contain only (1, 3, 2))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " was not equal to " + decorateToStringValue(List(3, 2, 1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(3, 2, 1)) and contain only (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was equal to " + decorateToStringValue(List(3, 2, 1)) + ", but " + decorateToStringValue(List(3, 2, 1)) + " did not contain only " + "(2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and contain only ("hello", "hi"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "hello")) and contain only ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(3, 2, 1)) and contain only (1, 3, 2))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List(3, 2, 1)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(3, 2, 1)) and contain only (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(List(3, 2, 1)) + " was equal to " + decorateToStringValue(List(3, 2, 1)) + ", but " + decorateToStringValue(List(3, 2, 1)) + " did not contain only " + "(2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "hello")) and contain only ("HELLO", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("HI", "HELLO")) and contain only ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "hello")) and contain only ("HO", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (List("hi", "hello")) and contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("HI", "HELLO")) and contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi", "hello")) and contain only ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")) + ", but " + decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HO\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(3, 2, 1)) and contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(3, 2, 1)) and contain only (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (be (Vector(Vector(3, 2, 1))) and contain only Vector(2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " was equal to " + decorateToStringValue(Vector(Vector(3, 2, 1))) + ", but " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " did not contain only (" + decorateToStringValue(Vector(2, 3, 8)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (be (Vector(Vector("hi", "hello"))) and contain only Vector("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " was equal to " + decorateToStringValue(Vector(Vector("hi", "hello"))) + ", but " + decorateToStringValue(Vector(Vector("hi", "hello"))) + " did not contain only (" + decorateToStringValue(Vector("ho", "hey", "howdy")) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (be (Vector(Vector(3, 2, 1))) and contain only Vector(2, 3, 8))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " was equal to " + decorateToStringValue(Vector(Vector(3, 2, 1))) + ", but " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " did not contain only (" + decorateToStringValue(Vector(2, 3, 8)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not contain only xx and not contain only xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain only (3, 2, 8) and not contain only (8, 3, 4))
        atLeast (2, lists) should (not contain only (3, 8, 5) and not contain only (8, 3, 4))
        atMost (2, lists) should (not contain only (2, 4, 3) and contain only (4, 3, 2))
        no (list1s) should (not contain only (1, 2, 3) and not contain only (1, 3, 2))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain only (2, 3, 4) and not contain only (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " contained only " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain only (3, 6, 8) and not contain only (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " did not contain only " + "(3, 6, 8)" + ", but " + decorateToStringValue(List(4, 3, 2)) + " contained only " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("hello", "hi") and not contain only ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("ho", "hey", "howdy") and not contain only ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"ho\", \"hey\", \"howdy\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain only ("HI") and not contain only ("HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("HELLO", "HI") and not contain only ("HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain only ("HI") and not contain only ("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HI\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain only ("HI") and not contain only ("HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain only ("HELLO", "HI") and not contain only ("HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain only ("HI") and not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " did not contain only " + "(\"HI\")" + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HELLO\", \"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only () and not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) and not contain only ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (3, 2, 2, 1) and not contain only (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain only (8, 3, 4) and not contain only (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (not contain only (Vector(3, 2, 1)) and not contain only (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + decorateToStringValue(Vector(3, 2, 1)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (not contain only (Vector(3, 6, 8)) and not contain only (Vector(3, 2, 1)))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " did not contain only (" + decorateToStringValue(Vector(3, 6, 8)) + "), did you forget to say : _*" + ", but " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + decorateToStringValue(Vector(3, 2, 1)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)

        val e3 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (not contain only (Vector("hi", "hello")) and not contain only ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " contained only (" + decorateToStringValue(Vector("hi", "hello")) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)

        val e4 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (not contain only (Vector("ho", "hey", "howdy")) and not contain only (Vector("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " did not contain only (" + decorateToStringValue(Vector("ho", "hey", "howdy")) + "), did you forget to say : _*, but " + decorateToStringValue(Vector(Vector("hi", "hello"))) + " contained only (" + decorateToStringValue(Vector("hi", "hello")) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not be (..) and not contain only (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (List(2)) and not contain only (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not contain only (8, 3, 4))
        atMost (2, lists) should (not be (List(4, 3, 2)) and not contain only (3, 4, 2))
        no (list1s) should (not be (List(3, 2, 1)) and not contain only (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(4, 3, 2)) and not contain only (8, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " was equal to " + decorateToStringValue(List(4, 3, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not contain only (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(4, 3, 2)) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(List(4, 3, 2)) + " contained only " + "(2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) and not contain only ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain only ("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"hello\", \"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) and not contain only ("HO", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) and not contain only ("HELLO", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain only ("HI", "HELLO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (List("ho")) and not contain only ("HO", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "hello")) and not contain only ("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was equal to " + decorateToStringValue(List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not contain only ("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi", "hello")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi", "hello")) + " contained only " + "(\"HI\", \"HELLO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS is empty") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) and not contain only ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyEmpty))
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) and not contain only (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.onlyDuplicate))
      }

      it("should throw TFE with friendly reminder when single GenTraversable argument is passed and failed") {
        val e1 = intercept[TestFailedException] {
          all (Vector(Vector(Vector(3, 2, 1)))) should (not be (List(3)) and not contain only (Vector(3, 2, 1)))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(Vector(Vector(3, 2, 1))) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(Vector(Vector(3, 2, 1))) + " contained only (" + decorateToStringValue(Vector(3, 2, 1)) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector(3, 2, 1)))), fileName, thisLineNumber - 2)

        val e2 = intercept[TestFailedException] {
          all (Vector(Vector(Vector("hi", "hello")))) should (not be (List("ho")) and not contain only (Vector("hi", "hello")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(Vector(Vector("hi", "hello"))) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(Vector(Vector("hi", "hello"))) + " contained only (" + decorateToStringValue(Vector("hi", "hello")) + "), did you forget to say : _*", thisLineNumber - 2, Vector(Vector(Vector("hi", "hello")))), fileName, thisLineNumber - 2)
      }
    }
  }
}
