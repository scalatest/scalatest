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
import scala.collection.mutable.ListBuffer
import exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldContainTheSameElementsInOrderAsLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  //ADDITIONAL//
  
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
  
  val fileName: String = "ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala"
  
  describe("a List") {
    
    val fumList: List[String] = List("fum", "foe", "fie", "fee")
    val toList: List[String] = List("you", "to", "birthday", "happy")
    
    describe("when used with (contain theSameElementsInOrderAs xx and contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") and contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }

      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsInOrderAs Set(" FUM ", " FOE ", " FIE ", " FEE ") and contain theSameElementsInOrderAs Set(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    describe("when used with (equal xx and contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (equal (fumList) and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FAM")))
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FAM"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (equal (toList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain theSameElementsInOrderAs Set(" FUM ", " FOE ", " FIE ", " FEE "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    describe("when used with (be xx and contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (be (fumList) and (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (be (toList) and contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) and contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE "))) (after being lowerCased and trimmed)
      }
    }

    describe("when used with (contain theSameElementsInOrderAs xx and be xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fum", "foe", "fie", "fee") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))) + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("fee", "fie", "foe", "fum") and be (fumList))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and be (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") and be (toList))
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain theSameElementsInOrderAs ListBuffer("HAPPY", "BIRTHDAY", "TO", "YOU") and (be (fumList)))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("HAPPY", "BIRTHDAY", "TO", "YOU"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and be (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FEE", "FIE", "FOE", "FUM") and be (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain theSameElementsInOrderAs ListBuffer("FUM", "FOE", "FIE", "FEE") and be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))) + ", but " + Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
        (fumList should (contain theSameElementsInOrderAs ListBuffer(" FUM ", " FOE ", " FIE ", " FEE ") and be (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    describe("when used with (not contain theSameElementsInOrderAs xx and not contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")) and not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")) and not contain theSameElementsInOrderAs (ListBuffer("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")) and not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fee", "fie", "foe", "fum"))) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) and (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")) and not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotContainSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FEE", "FIE", "FOE", "FUM"))) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not equal xx and not contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not equal (toList) and not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not equal (fumList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.didNotEqual(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.equaled(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not be xx and not contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        fumList should (not be (toList) and not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain theSameElementsInOrderAs (ListBuffer("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain theSameElementsInOrderAs (ListBuffer("fum", "foe", "fie", "fee")))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("fum", "foe", "fie", "fee"))), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE"))))
        }
        checkMessageStackDepth(e2, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (fumList should (not be (toList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain theSameElementsInOrderAs (ListBuffer("FUM", "FOE", "FIE", "FEE")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources.wasNotEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, toList)) + ", but " + Resources.containedSameElementsInOrder(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, ListBuffer("FUM", "FOE", "FIE", "FEE"))), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain theSameElementsInOrderAs (ListBuffer("FEE", "FIE", "FOE", "FUM")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources.wasEqualTo(decorateToStringValue(prettifier, fumList), decorateToStringValue(prettifier, fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain theSameElementsInOrderAs (ListBuffer(" FEE ", " FIE ", " FOE ", " FUU ")) and not contain theSameElementsInOrderAs (ListBuffer(" FEE ", " FIE ", " FOE ", " FUU ")))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
  }
  
  describe("col of Lists") {
    
    val list1s: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
    val lists: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), List(2, 3, 4))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1, 2, 3), List(1, 2, 3), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi", "hello"), List("hi", "hello"), List("hi", "hello"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + decorateToStringValue(prettifier, left)
    
    describe("used with contain theSameElementsInOrderAs xx and contain theSameElementsInOrderAs xx") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atLeast (2, lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atMost (2, lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        no (lists) should (contain theSameElementsInOrderAs ListBuffer(3, 6, 9) and contain theSameElementsInOrderAs ListBuffer(3, 4, 5))
        no (nils) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 8) and contain theSameElementsInOrderAs ListBuffer(1, 3, 4))
        no (listsNil) should (contain theSameElementsInOrderAs ListBuffer(3, 8, 5) and contain theSameElementsInOrderAs ListBuffer(3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (contain theSameElementsInOrderAs ListBuffer(1, 2, 3) and contain theSameElementsInOrderAs ListBuffer(1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List(1, 2, 3)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 2, 3)) + ", but " + decorateToStringValue(prettifier, List(1, 2, 3)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(1, 3, 4)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (contain theSameElementsInOrderAs ListBuffer("hi", "hello") and contain theSameElementsInOrderAs ListBuffer("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, nils(0)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("hi", "hello") and contain theSameElementsInOrderAs ListBuffer("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HO", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HO", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HO", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain theSameElementsInOrderAs ListBuffer("HI", "HELLO") and contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (be xx and contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atLeast (2, lists) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        atMost (2, lists) should (be (List(3, 2, 1)) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        no (lists) should (be (List(3, 6, 9)) and contain theSameElementsInOrderAs ListBuffer(3, 4, 5))
        no (nils) should (be (List(1, 6, 8)) and contain theSameElementsInOrderAs ListBuffer(1, 3, 4))
        no (listsNil) should (be (List(2, 6, 8)) and contain theSameElementsInOrderAs ListBuffer(3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2, 3, 4)) + " was not equal to " + decorateToStringValue(prettifier, List(1, 2, 3)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List(1, 2, 3)) + " was equal to " + decorateToStringValue(prettifier, List(1, 2, 3)) + ", but " + decorateToStringValue(prettifier, List(1, 2, 3)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and contain theSameElementsInOrderAs ListBuffer("hi", "hello"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, nils(0)) + " was not equal to " + decorateToStringValue(prettifier, List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "hello")) and contain theSameElementsInOrderAs ListBuffer("hello", "hi"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hello", "hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(1, 2, 3))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(prettifier, listsNil(2)) + " was not equal to " + decorateToStringValue(prettifier, List(1, 2, 3)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(1, 2, 3)) and contain theSameElementsInOrderAs ListBuffer(2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(prettifier, List(1, 2, 3)) + " was equal to " + decorateToStringValue(prettifier, List(1, 2, 3)) + ", but " + decorateToStringValue(prettifier, List(1, 2, 3)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 8)), thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi", "hello")) and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("HI", "HELLO")) and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi", "hello")) and contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (be (List("hi", "hello")) and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("HI", "HELLO")) and contain theSameElementsInOrderAs ListBuffer("HI", "HELLO"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, List("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi", "hello")) and contain theSameElementsInOrderAs ListBuffer("HELLO", "HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not contain theSameElementsInOrderAs xx and not contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not contain theSameElementsInOrderAs (ListBuffer(3, 2, 8)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        atLeast (2, lists) should (not contain theSameElementsInOrderAs (ListBuffer(3, 8, 5)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        atMost (2, lists) should (not contain theSameElementsInOrderAs (ListBuffer(2, 4, 3)) and contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)))
        no (list1s) should (not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)) and not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, List(2, 3, 4)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain theSameElementsInOrderAs (ListBuffer(3, 6, 8)) and not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, List(2, 3, 4)) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(3, 6, 8)) + ", but " + decorateToStringValue(prettifier, List(2, 3, 4)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")) and not contain theSameElementsInOrderAs (ListBuffer("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("ho", "hey", "howdy")) and not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("ho", "hey", "howdy")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) and not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) and not contain theSameElementsInOrderAs (ListBuffer("HO")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) and not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) and not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")) and not contain theSameElementsInOrderAs (ListBuffer("HO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")) and not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HELLO", "HI")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    describe("when used with (not be xx and not contain theSameElementsInOrderAs xx)") {
      
      it("should do nothing if valid, else throw a TFE with an appropriate error message") {
        all (list1s) should (not be (List(2)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        atLeast (2, lists) should (not be (List(3)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        atMost (2, lists) should (not be (List(2, 3, 4)) and not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)))
        no (list1s) should (not be (List(1, 2, 3)) and not contain theSameElementsInOrderAs (ListBuffer(1, 2, 3)))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(2, 3, 4)) and not contain theSameElementsInOrderAs (ListBuffer(8, 3, 4)))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was equal to " + decorateToStringValue(prettifier, List(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not contain theSameElementsInOrderAs (ListBuffer(2, 3, 4)))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(prettifier, lists(2)) + " was not equal to " + decorateToStringValue(prettifier, List(3)) + ", but " + decorateToStringValue(prettifier, List(2, 3, 4)) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer(2, 3, 4)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) and not contain theSameElementsInOrderAs (ListBuffer("ho", "hey", "howdy")))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain theSameElementsInOrderAs (ListBuffer("hi", "hello")))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(prettifier, hiLists(0)) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use the implicit Equality in scope") {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) and not contain theSameElementsInOrderAs (ListBuffer("HO", "HELLO")))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi", "hello")) and not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      it("should use an explicitly provided Equality") {
        (all (hiLists) should (not be (List("ho")) and not contain theSameElementsInOrderAs (ListBuffer("HO", "HELLO")))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi", "hello")) and not contain theSameElementsInOrderAs (ListBuffer("HELLO", "HI")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was equal to " + decorateToStringValue(prettifier, List("hi", "hello")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not contain theSameElementsInOrderAs (ListBuffer("HI", "HELLO")))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(prettifier, List("hi", "hello")) + " was not equal to " + decorateToStringValue(prettifier, List("ho")) + ", but " + decorateToStringValue(prettifier, List("hi", "hello")) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(prettifier, ListBuffer("HI", "HELLO")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
