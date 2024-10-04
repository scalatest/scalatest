/*
* Copyright 2001-2024 Artima, Inc.
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

class ListShouldContainAtLeastOneOfLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  val upperCaseStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = upperCase(a) == b
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
  
  val fileName: String = "ListShouldContainAtLeastOneOfLogicalAndSpec.scala"
  
  describe("a List") {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    describe("when used with (contain oneOf (..) and contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and contain atLeastOneOf ("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "foe") and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and (contain atLeastOneOf ("fum", "foe")))
        }
        checkMessageStackDepth(e2, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and contain atLeastOneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "foe") and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fie", "fum") and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (equal (..) and contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain atLeastOneOf ("fum", "foe")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (equal (fumList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (equal (fumList) and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by defaultEquality, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) and contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (be (..) and contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain atLeastOneOf ("fum", "foe")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) and contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }

    describe("when used with (contain oneOf (..) and be (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "foe") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "foe") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "foe") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and be (fumList))) (after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf("fee", "fie", "foe", "fie", "fum") and be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (not contain oneOf (..) and not contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fuu") and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fum") and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atLeastOneOf ("fum", "foe") and not contain atLeastOneOf ("fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and not contain atLeastOneOf ("fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("fum", "foe") and (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\"") + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not contain atLeastOneOf ("fum", "foe") and not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("fum", "foe") and not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fum\", \"foe\"") + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") and not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fie", "fum") and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneOf ("fie", "fee", "fuu", "foe") and not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (not equal (..) and not contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain atLeastOneOf ("fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneOf ("fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not equal (toList) and not contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) and not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (not be (..) and not contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain atLeastOneOf ("fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atLeastOneOf ("fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedAtLeastOneOf(decorateToStringValue(prettifier, fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) and not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
  }
  
  describe("collection of Lists") {
    
    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(prettifier, left)
    
    describe("when used with (contain oneOf (..) and contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain atLeastOneOf (3, 2, 1) and contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneOf (3, 1, 5) and contain atLeastOneOf (1, 3, 4))
        atMost (2, lists) should (contain atLeastOneOf (3, 2, 8) and contain atLeastOneOf (2, 3, 4))
        no (lists) should (contain atLeastOneOf (3, 6, 9) and contain atLeastOneOf (3, 4, 5))
        no (nils) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        no (listsNil) should (contain atLeastOneOf(3, 8, 5) and contain atLeastOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 6, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " did not contain at least one of (1, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " contained at least one of (1, 2, 8), but " + decorateToStringValue(prettifier, List(2)) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (contain atLeastOneOf ("hi", "hello") and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Nil) + " did not contain at least one of (\"hi\", \"hello\")", thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi", "hello") and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"hi\", \"hello\"), but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (contain atLeastOneOf (1, 3, 4) and contain atLeastOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(prettifier, Nil) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e6, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " contained at least one of (1, 2, 8), but " + decorateToStringValue(prettifier, List(2)) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain atLeastOneOf ("HI", "HE") and contain atLeastOneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi", "he") and contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("HI", "HE") and contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\"), but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain atLeastOneOf ("HI", "HE") and contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("hi", "he") and contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("HI", "HE") and contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\"), but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf (3, 2, 2, 1) and contain atLeastOneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf (1, 3, 4) and contain atLeastOneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (be (..) and contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        atMost (2, lists) should (be (List(1)) and contain atLeastOneOf (2, 3, 4))
        no (lists) should (be (List(8)) and contain atLeastOneOf (3, 4, 5))
        no (nils) should (be (List(8)) and contain atLeastOneOf (1, 3, 4))
        no (listsNil) should (be (List(8)) and contain atLeastOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " was not equal to " + decorateToStringValue(prettifier, List(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and contain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List(1)) + " was equal to " + decorateToStringValue(prettifier, List(1)) + ", but " + decorateToStringValue(prettifier, List(1)) + " did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, Nil) + " was not equal to " + decorateToStringValue(prettifier, List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(1)) and contain atLeastOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(prettifier, Nil) + " was not equal to " + decorateToStringValue(prettifier, List(1)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and contain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(prettifier, List(1)) + " was equal to " + decorateToStringValue(prettifier, List(1)) + ", but " + decorateToStringValue(prettifier, List(1)) + " did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) and contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) and contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(1)) and contain atLeastOneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (not contain oneOf (..) and not contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain atLeastOneOf (3, 2, 8) and not contain atLeastOneOf (8, 3, 4))
        atLeast (2, lists) should (not contain atLeastOneOf (3, 8, 5) and not contain atLeastOneOf (8, 3, 4))
        atMost (2, lists) should (not contain atLeastOneOf (3, 6, 8) and contain atLeastOneOf (5, 3, 4))
        no (lists) should (not contain atLeastOneOf (1, 2, 9) and not contain atLeastOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneOf (2, 6, 8) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " contained at least one of (2, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneOf (3, 6, 8) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " did not contain at least one of (3, 6, 8), but " + decorateToStringValue(prettifier, List(2)) + " contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("hi", "hello") and not contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("ho", "hey", "howdy") and not contain atLeastOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\"), but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain atLeastOneOf ("hi", "he") and not contain atLeastOneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("HI", "HE") and not contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("hi", "he") and not contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\"), but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain atLeastOneOf ("hi", "he") and not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("HI", "HE") and not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("hi", "he") and not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " did not contain at least one of (\"hi\", \"he\"), but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneOf (3, 2, 2, 1) and not contain atLeastOneOf (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneOf (8, 3, 4) and not contain atLeastOneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
    
    describe("when used with (not be (..) and not contain oneOf (..))") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (List(2)) and not contain atLeastOneOf (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not contain atLeastOneOf (8, 3, 4))
        atMost (2, lists) should (not be (List(3)) and contain atLeastOneOf (5, 3, 4))
        no (list1s) should (not be (List(1)) and not contain atLeastOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(2)) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " was equal to " + decorateToStringValue(prettifier, List(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, List(2)) + " was not equal to " + decorateToStringValue(prettifier, List(3)) + ", but " + decorateToStringValue(prettifier, List(2)) + " contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was equal to " + decorateToStringValue(prettifier, List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value") {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) and not contain atLeastOneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources.atLeastOneOfDuplicate))
      }
    }
  }
}
