/*
* Copyright 2001-2025 Artima, Inc.
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
import org.scalactic.Prettifier
import org.scalactic.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldContainAtMostOneOfLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == upperCase(b)
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
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainAtMostOneOfLogicalAndSpec.scala"
  
  describe("a List") {
    
    val fumList: List[String] = List("fum", "foe")
    val toList: List[String] = List("to", "you")
    
    describe("when used with (contain atMostOneOf (..) and contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atMostOneOf ("fee", "fie", "foe", "fam") and contain atMostOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fum") and contain atMostOneOf ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fam") and contain atMostOneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e2, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and (contain atMostOneOf ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM ") and contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fie", "fum") and contain atMostOneOf("fie", "fee", "fam", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneOf ("fie", "fee", "fam", "foe") and contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (equal (..) and contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain atMostOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atMostOneOf ("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain atMostOneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain atMostOneOf ("FIE", "FEE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (be (..) and contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain atMostOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain atMostOneOf ("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain atMostOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain atMostOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain atMostOneOf ("FEE", "FIE", "FOE", "FAM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }

    describe("when used with (contain atMostOneOf (..) and be (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atMostOneOf("fie", "fee", "fam", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fam") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("fee", "fie", "foe", "fum") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and (be (toList)))
        }
        checkMessageStackDepth(e2, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM ") and be (fumList))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atMostOneOf("fee", "fie", "foe", "fie", "fum") and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (not contain atMostOneOf (..) and not contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fum") and not contain atMostOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fuu") and not contain atMostOneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fuu\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fum") and not contain atMostOneOf ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUU") and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUU\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and (not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FUM") and not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain atMostOneOf ("FEE", "FIE", "FOE", "FAM") and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FAM\""), fileName, thisLineNumber - 2)
        (fumList should (contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM ") and contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atMostOneOf ("fee", "fie", "foe", "fie", "fum") and not contain atMostOneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atMostOneOf ("fie", "fee", "fum", "foe") and not contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (not equal (..) and not contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain atMostOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atMostOneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain atMostOneOf ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (not be (..) and not contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain atMostOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atMostOneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain atMostOneOf ("fie", "fee", "fam", "foe"))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"fie\", \"fee\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain atMostOneOf ("FIE", "FEE", "FAM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtMostOneOf(decorateToStringValue(prettifier, fumList), "\"FIE\", \"FEE\", \"FAM\", \"FOE\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain atMostOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and not contain atMostOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain atMostOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
  }
  
  describe("collection of Lists") {
    
    val list1s: Vector[List[Int]] = Vector(List(1, 2), List(1, 2), List(1, 2))
    val lists: Vector[List[Int]] = Vector(List(1, 2), List(1, 2), List(2, 3))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "he"), List("hi", "he"), List("hi", "he"))
    val toLists: Vector[List[String]] = Vector(List("to", "you"), List("to", "you"), List("to", "you"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(prettifier, left)
    
    describe("when used with (contain atMostOneOf (..) and contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain atMostOneOf (3, 4, 1) and contain atMostOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atMostOneOf (3, 2, 5) and contain atMostOneOf (2, 3, 4))
        atMost (2, lists) should (contain atMostOneOf (3, 2, 8) and contain atMostOneOf (2, 3, 4))
        no (lists) should (contain atMostOneOf (1, 2, 3) and contain atMostOneOf (3, 2, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneOf (2, 3, 4) and contain atMostOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneOf (1, 3, 4) and contain atMostOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " contained at most one of (1, 3, 4), but " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneOf ("hi", "hello") and contain atMostOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"hi\", \"hello\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (lists) should (contain atMostOneOf (1, 6, 8) and contain atMostOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e6, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " contained at most one of (1, 6, 8), but " + decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain atMostOneOf ("HI", "HO") and contain atMostOneOf ("HE", "HO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneOf ("HI", "HE") and contain atMostOneOf ("HE", "HO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain atMostOneOf ("HI", "HO") and contain atMostOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain atMostOneOf ("HI", "HO") and contain atMostOneOf ("HE", "HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneOf ("HI", "HE") and contain atMostOneOf ("HE", "HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain atMostOneOf ("HI", "HO") and contain atMostOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atMostOneOf (1, 2, 2, 3) and contain atMostOneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atMostOneOf (1, 3, 4) and contain atMostOneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (be (..) and contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (List(1, 2)) and contain atMostOneOf (1, 3, 4))
        atLeast (2, lists) should (be (List(1, 2)) and contain atMostOneOf (2, 3, 4))
        atMost (2, lists) should (be (List(1, 2)) and contain atMostOneOf (2, 3, 4))
        no (lists) should (be (List(8)) and contain atMostOneOf (3, 2, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(1, 2)) and contain atMostOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was not equal to " + decorateToStringValue(prettifier, List(1, 2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(1, 2)) and contain atMostOneOf (1, 2, 3))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was equal to " + decorateToStringValue(prettifier, List(1, 2)) + ", but " + decorateToStringValue(prettifier, list1s(0)) + " did not contain at most one of (1, 2, 3)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "he")) and contain atMostOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(1, 2)) and contain atMostOneOf (1, 2, 3))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(prettifier, list1s(0)) + " was equal to " + decorateToStringValue(prettifier, List(1, 2)) + ", but " + decorateToStringValue(prettifier, list1s(0)) + " did not contain at most one of (1, 2, 3)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "he")) and contain atMostOneOf ("HO", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) and contain atMostOneOf ("HO", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "he")) and contain atMostOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (List("hi", "he")) and contain atMostOneOf ("HO", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) and contain atMostOneOf ("HO", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi", "he")) and contain atMostOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(1, 2)) and contain atMostOneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (not contain atMostOneOf (..) and not contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain atMostOneOf (3, 2, 1) and not contain atMostOneOf (1, 2, 3))
        atLeast (2, lists) should (not contain atMostOneOf (1, 2, 8) and not contain atMostOneOf (1, 2, 3))
        atMost (2, lists) should (not contain atMostOneOf (1, 2, 8) and contain atMostOneOf (1, 2, 3))
        no (lists) should (not contain atMostOneOf (2, 6, 8) and not contain atMostOneOf (3, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atMostOneOf (1, 2, 8) and not contain atMostOneOf (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " contained at most one of (1, 2, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain atMostOneOf (1, 2, 3) and not contain atMostOneOf (1, 2, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " did not contain at most one of (1, 2, 3), but " + decorateToStringValue(prettifier, lists(2)) + " contained at most one of (1, 2, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain atMostOneOf ("hi", "hello") and not contain atMostOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain atMostOneOf ("hi", "he") and not contain atMostOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"hi\", \"he\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain atMostOneOf ("HI", "HE") and not contain atMostOneOf ("HE", "HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atMostOneOf ("HI", "HO") and not contain atMostOneOf ("HE", "HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain atMostOneOf ("HI", "HE") and not contain atMostOneOf ("HI", "HO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain atMostOneOf ("HI", "HE") and not contain atMostOneOf ("HE", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atMostOneOf ("HI", "HO") and not contain atMostOneOf ("HE", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atMostOneOf ("HI", "HE") and not contain atMostOneOf ("HI", "HO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " did not contain at most one of (\"HI\", \"HE\"), but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atMostOneOf (1, 2, 2, 3) and not contain atMostOneOf (1, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atMostOneOf (1, 2, 3) and not contain atMostOneOf (1, 2, 2, 3))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
    
    describe("when used with (not be (..) and not contain atMostOneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (List(2, 3)) and not contain atMostOneOf (1, 2, 3))
        atLeast (2, lists) should (not be (List(2, 3)) and not contain atMostOneOf (1, 2, 3))
        atMost (2, lists) should (not be (List(2, 3)) and contain atMostOneOf (1, 2, 3))
        no (list1s) should (not be (List(1, 2)) and not contain atMostOneOf (3, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(2, 3)) and not contain atMostOneOf (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was equal to " + decorateToStringValue(prettifier, List(2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(2, 3, 4)) and not contain atMostOneOf (1, 2, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was not equal to " + decorateToStringValue(prettifier, List(2, 3, 4)) + ", but " + decorateToStringValue(prettifier, lists(2)) + " contained at most one of (1, 2, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "he")) and not contain atMostOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "ho")) and not contain atMostOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("hi", "ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("HI", "HO")) and not contain atMostOneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "he")) and not contain atMostOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "ho")) and not contain atMostOneOf ("HI", "HO"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("hi", "ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (List("HI", "HO")) and not contain atMostOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "he")) and not contain atMostOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "he")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "ho")) and not contain atMostOneOf ("HI", "HO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("hi", "ho")) + ", but " + decorateToStringValue(prettifier, hiLists(0)) + " contained at most one of (\"HI\", \"HO\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }

      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2, 3)) and not contain atMostOneOf (1, 2, 2, 3))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atMostOneOfDuplicate))
      }
    }
  }
}
