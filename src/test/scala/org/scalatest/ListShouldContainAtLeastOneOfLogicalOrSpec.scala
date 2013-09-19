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

class ListShouldContainAtLeastOneOfLogicalOrSpec extends FreeSpec with Matchers {

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
  
  val fileName: String = "ListShouldContainAtLeastOneOfLogicalOrSpec.scala"
  
  "a List" - {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
      "when used with (contain oneOf (...) or contain oneOf (...)) syntax" - {
      
        "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fam") or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") or contain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fam") or contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or contain atLeastOneOf ("FIE", "FEE", "FOE", "FUM"))
        fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf ("FIE", "FEE", "FOE", "FUM"))
        fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or contain atLeastOneOf ("fie", "fee", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "foe") or (contain atLeastOneOf ("fie", "fee", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or contain atLeastOneOf ("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf ("FIE", "FEE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or contain atLeastOneOf ("fie", "fee", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "foe") or contain atLeastOneOf ("fie", "fee", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fie\", \"fee\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf () or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fie", "fum") or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (equal (...) and contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (equal (fumList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or contain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (toList) or contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (equal (fumList) or contain atLeastOneOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain atLeastOneOf ("fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (equal (fumList) or contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) or contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by defaultEquality, after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain atLeastOneOf())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (equal (fumList) or contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (be (...) and contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (be (fumList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (be (toList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (be (fumList) or contain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (be (fumList) or contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (toList) or contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (be (fumList) or contain atLeastOneOf ("fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (be (toList) or (contain atLeastOneOf ("fum", "foe")))
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (be (fumList) or contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (toList) or contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality)
        (fumList should (be (fumList) or contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (be (toList) or contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (be (fumList) or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain atLeastOneOf())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (be (fumList) or contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }

    "when used with (contain oneOf (...) and be (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") or be (fumList))
        fumList should (contain atLeastOneOf("fie", "fee", "fam", "foe") or be (fumList))
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fam") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or be (fumList))
        fumList should (contain atLeastOneOf ("fum", "foe") or be (fumList))
        fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or be (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "foe") or be (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneOf ("fum", "foe") or be (fumList))) (decided by upperCaseStringEquality)
        (fumList should (contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or be (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "foe") or be (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"foe\"") + ", and " + Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or be (fumList))) (after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf() or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (contain atLeastOneOf("fee", "fie", "foe", "fie", "fum") or be (fumList))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (not contain oneOf (...) and not contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fuu") or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fum") or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fuu") or not contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fum") or not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not contain atLeastOneOf ("fum", "foe") or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not contain atLeastOneOf ("fum", "foe") or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") or not contain atLeastOneOf ("FEE", "FIE", "FUM", "FOE"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not contain atLeastOneOf ("fum", "foe") or not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE") or not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not contain atLeastOneOf ("fum", "foe") or not contain atLeastOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM") or not contain atLeastOneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fie", "fum") or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          fumList should (not contain atLeastOneOf ("fie", "fee", "fuu", "foe") or not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (not equal (...) and not contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not equal (toList) or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not equal (fumList) or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not equal (toList) or not contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not equal (fumList) or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not equal (toList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not equal (toList) or not contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not contain atLeastOneOf ("fum", "foe"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by defaultEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not equal (toList) or not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (not be (...) and not contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (not be (toList) or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not be (fumList) or not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not be (toList) or not contain atLeastOneOf("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not be (fumList) or not contain atLeastOneOf ("fum", "foe"))
        fumList should (not be (toList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not be (toList) or not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not contain atLeastOneOf ("fum", "foe"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain atLeastOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          fumList should (not be (toList) or not contain atLeastOneOf("fee", "fie", "foe", "fie", "fum"))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
  }
  
  "collection of Lists" - {
    
    val list1s: Vector[List[Int]] = Vector(List(1), List(1), List(1))
    val lists: Vector[List[Int]] = Vector(List(1), List(1), List(2))
    val nils: Vector[List[Int]] = Vector(Nil, Nil, Nil)
    val listsNil: Vector[List[Int]] = Vector(List(1), List(1), Nil)
    val hiLists: Vector[List[String]] = Vector(List("hi"), List("hi"), List("hi"))
    val toLists: Vector[List[String]] = Vector(List("to"), List("to"), List("to"))
    
    def allErrMsg(index: Int, message: String, lineNumber: Int, left: Any): String = 
      "'all' inspection failed, because: \n" +
      "  at index " + index + ", " + message + " (" + fileName + ":" + (lineNumber) + ") \n" +
      "in " + left
    
    "when used with (contain oneOf (..) and contain oneOf (..)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (contain atLeastOneOf (3, 2, 1) or contain atLeastOneOf (1, 3, 4))
        all (list1s) should (contain atLeastOneOf (3, 2, 5) or contain atLeastOneOf (1, 3, 4))
        all (list1s) should (contain atLeastOneOf (3, 2, 1) or contain atLeastOneOf (2, 3, 4))
        
        atLeast (2, lists) should (contain atLeastOneOf (3, 1, 5) or contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneOf (3, 6, 5) or contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneOf (3, 1, 5) or contain atLeastOneOf (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (6, 7, 8) or contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " did not contain at least one of (6, 7, 8), and " + decorateToStringValue(List(2)) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (contain atLeastOneOf ("HI", "HE") or contain atLeastOneOf ("HI", "HE"))
        all (hiLists) should (contain atLeastOneOf ("hi", "he") or contain atLeastOneOf ("HI", "HE"))
        all (hiLists) should (contain atLeastOneOf ("HI", "HE") or contain atLeastOneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi", "he") or contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\"), and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (contain atLeastOneOf ("HI", "HE") or contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atLeastOneOf ("hi", "he") or contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (contain atLeastOneOf ("HI", "HE") or contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("hi", "he") or contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\"), and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf () or contain atLeastOneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf (3, 2, 1) or contain atLeastOneOf ())
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf (3, 2, 2, 1) or contain atLeastOneOf (1, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (contain atLeastOneOf (1, 3, 4) or contain atLeastOneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (be (...) and contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (be (List(1)) or contain atLeastOneOf (1, 3, 4))
        all (list1s) should (be (List(2)) or contain atLeastOneOf (1, 3, 4))
        all (list1s) should (be (List(1)) or contain atLeastOneOf (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2)) or contain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was not equal to " + decorateToStringValue(List(2)) + ", and " + decorateToStringValue(List(1)) + " did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("HI", "HE"))
        all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("HI", "HE"))
        all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("hi", "he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("hi", "he"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\", \"he\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS is empty" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(1)) or contain atLeastOneOf ())
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfEmpty")))
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (be (List(1)) or contain atLeastOneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (not contain oneOf (..) and not contain oneOf (..))" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not contain atLeastOneOf (3, 2, 8) or not contain atLeastOneOf (8, 3, 4))
        all (list1s) should (not contain atLeastOneOf (1, 2, 8) or not contain atLeastOneOf (8, 3, 4))
        all (list1s) should (not contain atLeastOneOf (3, 2, 8) or not contain atLeastOneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneOf (2, 6, 8) or not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " contained at least one of (2, 6, 8), and " + decorateToStringValue(List(2)) + " contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not contain atLeastOneOf ("hi", "he") or not contain atLeastOneOf ("hi", "he"))
        all (hiLists) should (not contain atLeastOneOf ("HI", "HE") or not contain atLeastOneOf ("hi", "he"))
        all (hiLists) should (not contain atLeastOneOf ("hi", "he") or not contain atLeastOneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("HI", "HE") or not contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\"), and " + decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not contain atLeastOneOf ("hi", "he") or not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atLeastOneOf ("HI", "HE") or not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not contain atLeastOneOf ("hi", "he") or not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("HI", "HE") or not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\"), and " + decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneOf (3, 2, 2, 1) or not contain atLeastOneOf (8, 3, 4))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
        
        val e2 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not contain atLeastOneOf (8, 3, 4) or not contain atLeastOneOf (3, 2, 2, 1))
        }
        e2.failedCodeFileName.get should be (fileName)
        e2.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e2.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
    
    "when used with (not be (...) and not contain oneOf (...))" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        all (list1s) should (not be (List(2)) or not contain atLeastOneOf (8, 3, 4))
        all (list1s) should (not be (List(1)) or not contain atLeastOneOf (8, 3, 4))
        all (list1s) should (not be (List(2)) or not contain atLeastOneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(1)) or not contain atLeastOneOf (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", and " + decorateToStringValue(List(1)) + " contained at least one of (2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("hi", "he"))
        all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("hi", "he"))
        all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("HI", "HE"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("HI", "HE"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("hi", "he"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("HI", "HE"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained at least one of (\"HI\", \"HE\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should throw NotAllowedException with correct stack depth and message when RHS contain duplicated value" in {
        val e1 = intercept[exceptions.NotAllowedException] {
          all (list1s) should (not be (List(2)) or not contain atLeastOneOf (3, 2, 2, 1))
        }
        e1.failedCodeFileName.get should be (fileName)
        e1.failedCodeLineNumber.get should be (thisLineNumber - 3)
        e1.message should be (Some(Resources("atLeastOneOfDuplicate")))
      }
    }
  }
}
