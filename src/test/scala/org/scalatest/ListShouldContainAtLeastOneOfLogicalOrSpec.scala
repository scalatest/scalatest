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
import org.scalautils.Normality
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainAtLeastOneOfLogicalOrSpec extends FreeSpec with Matchers {
  
  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val invertedListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
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
        implicit val ise = invertedStringEquality
        fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") or contain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") or contain atLeastOneOf ("fie", "fee", "fum", "foe"))
        fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "fum", "fum", "fum") or (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") or contain atLeastOneOf ("fie", "fee", "fum", "foe"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (contain atLeastOneOf ("fie", "fee", "fum", "foe") or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "fum") or contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
        implicit val ise = invertedStringEquality
        fumList should (equal (fumList) or contain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (equal (toList) or contain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (equal (fumList) or contain atLeastOneOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (equal (toList) or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (equal (fumList) or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (equal (toList) or contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    "when used with (legacyEqual (...) and contain oneOf (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (legacyEqual (fumList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (toList) or contain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (fumList) or contain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = invertedStringEquality
        fumList should (legacyEqual (fumList) or contain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (legacyEqual (toList) or contain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (legacyEqual (fumList) or contain atLeastOneOf ("fum", "fum", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (legacyEqual (fumList) or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumList should (legacyEqual (toList) or contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumList should (legacyEqual (fumList) or contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    "when used with (contain oneOf (...) and legacyEqual (...)) syntax" - {
      
      "should do nothing if valid, else throw a TFE with an appropriate error message" in {
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") or legacyEqual (fumList))
        fumList should (contain atLeastOneOf("fie", "fee", "fam", "foe") or legacyEqual (fumList))
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fam") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      "should use the implicit Equality in scope" in {
        implicit val ise = invertedStringEquality
        fumList should (contain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (fumList))
        fumList should (contain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (fumList))
        fumList should (contain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (contain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (fumList))) (decided by invertedStringEquality)
        (fumList should (contain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (fumList))) (decided by invertedStringEquality)
        (fumList should (contain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (toList))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (toList))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
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
        implicit val ise = invertedStringEquality
        fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") or not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") or not contain atLeastOneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"have\", \"a\", \"nice\", \"day\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") or not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") or not contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"have\", \"a\", \"nice\", \"day\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
        implicit val ise = invertedStringEquality
        fumList should (not equal (toList) or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not equal (fumList) or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not equal (toList) or not contain atLeastOneOf ("fie", "fee", "fuu", "foe"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not contain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not equal (fumList) or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (not equal (toList) or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (not equal (fumList) or not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
        implicit val ise = invertedStringEquality
        fumList should (not be (toList) or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not be (fumList) or not contain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not be (toList) or not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not contain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (fumList should (not be (toList) or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        (fumList should (not be (fumList) or not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        (fumList should (not be (toList) or not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (contain atLeastOneOf ("ho") or contain atLeastOneOf ("he"))
        all (hiLists) should (contain atLeastOneOf ("hi") or contain atLeastOneOf ("he"))
        all (hiLists) should (contain atLeastOneOf ("ho") or contain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi") or contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\"), and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (contain atLeastOneOf ("ho") or contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (contain atLeastOneOf ("hi") or contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (contain atLeastOneOf ("ho") or contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("hi") or contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\"), and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
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
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("he"))
        all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("he"))
        all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        (all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        (all (hiLists) should (be (List("hi")) or contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) or contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
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
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not contain atLeastOneOf ("hi") or not contain atLeastOneOf ("hi"))
        all (hiLists) should (not contain atLeastOneOf ("ho") or not contain atLeastOneOf ("hi"))
        all (hiLists) should (not contain atLeastOneOf ("hi") or not contain atLeastOneOf ("h0"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("ho") or not contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\"), and " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not contain atLeastOneOf ("hi") or not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (not contain atLeastOneOf ("ho") or not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (not contain atLeastOneOf ("hi") or not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("ho") or not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\"), and " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
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
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("hi"))
        all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("hi"))
        all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      "should use an explicitly provided Equality" in {
        (all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        (all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        (all (hiLists) should (not be (List("ho")) or not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) or not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
