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
import scala.collection.JavaConverters._

class ListShouldContainNoneOfLogicalOrSpec extends Spec with Matchers {
  
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
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainNoneOfLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain noneOf (..) or contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fam") or newContain newNoneOf("fie", "fee", "fam", "foe"))
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fam") or newContain newNoneOf("fie", "fee", "fum", "foe"))
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf ("fee", "fie", "fum", "foe"))
        fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or newContain newNoneOf ("fee", "fie", "fum", "foe"))
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or (newContain newNoneOf ("FEE", "FIE", "FUM", "FOE")))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or newContain newNoneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FUM\", \"FOE\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FAM ") or newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or newContain newNoneOf("fie", "fee", "fam", "foe"))
        fumList should (equal (toList) or newContain newNoneOf("fie", "fee", "fam", "foe"))
        fumList should (equal (fumList) or newContain newNoneOf("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or newContain newNoneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) or newContain newNoneOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or newContain newNoneOf ("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or newContain newNoneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (fumList) or newContain newNoneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (equal (toList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or newContain newNoneOf("fie", "fee", "fam", "foe"))
        fumList should (legacyEqual (toList) or newContain newNoneOf("fie", "fee", "fam", "foe"))
        fumList should (legacyEqual (fumList) or newContain newNoneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or newContain newNoneOf ("fie", "fee", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) or newContain newNoneOf ("fee", "fie", "foe", "fum"))
        fumList should (legacyEqual (toList) or newContain newNoneOf ("fee", "fie", "foe", "fum"))
        fumList should (legacyEqual (fumList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or newContain newNoneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (toList) or newContain newNoneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        (fumList should (legacyEqual (fumList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FAM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain noneOf (..) and legacyEqual (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newNoneOf("fie", "fee", "fam", "foe") or legacyEqual (fumList))
        fumList should (newContain newNoneOf("fie", "fee", "fum", "foe") or legacyEqual (fumList))
        fumList should (newContain newNoneOf("fie", "fee", "fam", "foe") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newNoneOf ("fie", "fee", "fum", "foe") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or legacyEqual (fumList))
        fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))
        fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or legacyEqual (fumList))) (decided by upperCaseStringEquality)
        (fumList should (newContain newNoneOf ("fee", "fie", "foe", "fum") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", and " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FUM ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain noneOf (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf("fee", "fie", "fum", "foe"))
        fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fam") or not newContain newNoneOf("fee", "fie", "fum", "foe"))
        fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf("fee", "fie", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fam") or not newContain newNoneOf ("fee", "fie", "fam", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fam\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or not newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))
        fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))
        fumList should (not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or not newContain newNoneOf ("fee", "fie", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf ("fee", "fie", "fum", "foe"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or not newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf ("FEE", "FIE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (fumList should (not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM") or not newContain newNoneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newNoneOf ("fee", "fie", "foe", "fum") or not newContain newNoneOf ("fee", "fie", "fum", "foe"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not equal (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not newContain newNoneOf("fee", "fie", "foe", "fum"))
        fumList should (not equal (fumList) or not newContain newNoneOf("fee", "fie", "foe", "fum"))
        fumList should (not equal (toList) or not newContain newNoneOf("fee", "fie", "foe", "fam"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not newContain newNoneOf ("fee", "fie", "foe", "fam"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fam\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) or not newContain newNoneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (not equal (fumList) or not newContain newNoneOf ("FIE", "FEE", "FUM", "FOE"))
        fumList should (not equal (toList) or not newContain newNoneOf ("fie", "fee", "fum", "foe"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not newContain newNoneOf ("fie", "fee", "fum", "foe")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not newContain newNoneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (toList) or not newContain newNoneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        (fumList should (not equal (fumList) or not newContain newNoneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not newContain newNoneOf ("fie", "fee", "fum", "foe"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FUM ") or not newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not newContain newNoneOf("fee", "fie", "foe", "fum"))
        fumList should (not be (fumList) or not newContain newNoneOf("fee", "fie", "foe", "fum"))
        fumList should (not be (toList) or not newContain newNoneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) or not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (fumList) or not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))
        fumList should (not be (toList) or not newContain newNoneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not newContain newNoneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (fumList) or not newContain newNoneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        (fumList should (not be (toList) or not newContain newNoneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not newContain newNoneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", and " + Resources("didNotContainOneOfElements", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FUM ") or not newContain newNoneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
      "in " + left
    
    object `when used with (contain noneOf (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newNoneOf (3, 6, 9) or newContain newNoneOf (2, 6, 8))
        all (list1s) should (newContain newNoneOf (1, 2, 3) or newContain newNoneOf (2, 6, 8))
        all (list1s) should (newContain newNoneOf (3, 6, 9) or newContain newNoneOf (1, 2, 3))
        
        atLeast (2, lists) should (newContain newNoneOf (2, 6, 8) or newContain newNoneOf (3, 6, 9))
        atLeast (2, lists) should (newContain newNoneOf (1, 2, 3) or newContain newNoneOf (3, 6, 9))
        atLeast (2, lists) should (newContain newNoneOf (2, 6, 8) or newContain newNoneOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newNoneOf (2, 6, 8) or newContain newNoneOf (2, 3, 5))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " contained one of (2, 6, 8), and " + decorateToStringValue(List(2)) + " contained one of (2, 3, 5)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (newContain newNoneOf ("hi") or newContain newNoneOf ("hi"))
        all (hiLists) should (newContain newNoneOf ("hi") or newContain newNoneOf ("HI"))
        all (hiLists) should (newContain newNoneOf ("HI") or newContain newNoneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newNoneOf ("HI") or newContain newNoneOf ("HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained one of (\"HI\"), and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newNoneOf ("hi") or newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newNoneOf ("hi") or newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (newContain newNoneOf ("HI") or newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newNoneOf ("HI") or newContain newNoneOf ("HI"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained one of (\"HI\"), and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1)) or newContain newNoneOf (2, 6, 8))
        all (list1s) should (be (List(2)) or newContain newNoneOf (2, 6, 8))
        all (list1s) should (be (List(1)) or newContain newNoneOf (1, 2, 3))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2)) or newContain newNoneOf (1, 2, 3))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was not equal to " + decorateToStringValue(List(2)) + ", and " + decorateToStringValue(List(1)) + " contained one of (1, 2, 3)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (be (List("hi")) or newContain newNoneOf ("hi"))
        all (hiLists) should (be (List("ho")) or newContain newNoneOf ("hi"))
        all (hiLists) should (be (List("hi")) or newContain newNoneOf ("HI"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) or newContain newNoneOf ("HI"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi")) or newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("ho")) or newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (be (List("hi")) or newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) or newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", and " + decorateToStringValue(List("hi")) + " contained one of (\"HI\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain noneOf (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newNoneOf (1, 2, 3) or not newContain newNoneOf (1, 6, 8))
        all (list1s) should (not newContain newNoneOf (2, 6, 8) or not newContain newNoneOf (1, 6, 8))
        all (list1s) should (not newContain newNoneOf (1, 2, 3) or not newContain newNoneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newNoneOf (1, 6, 8) or not newContain newNoneOf (1, 3, 5))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " did not contain one of (1, 6, 8), and " + decorateToStringValue(List(2)) + " did not contain one of (1, 3, 5)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not newContain newNoneOf ("HI") or not newContain newNoneOf ("HI"))
        all (hiLists) should (not newContain newNoneOf ("hi") or not newContain newNoneOf ("HI"))
        all (hiLists) should (not newContain newNoneOf ("HI") or not newContain newNoneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newNoneOf ("hi") or not newContain newNoneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain one of (\"hi\"), and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newNoneOf ("HI") or not newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newNoneOf ("hi") or not newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        (all (hiLists) should (not newContain newNoneOf ("HI") or not newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newNoneOf ("hi") or not newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain one of (\"hi\"), and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (..) and not contain noneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not newContain newNoneOf (1, 6, 8))
        all (list1s) should (not be (List(1)) or not newContain newNoneOf (1, 6, 8))
        all (list1s) should (not be (List(2)) or not newContain newNoneOf (2, 6, 8))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(1)) or not newContain newNoneOf (2, 6, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", and " + decorateToStringValue(List(1)) + " did not contain one of (2, 6, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        
        all (hiLists) should (not be (List("ho")) or not newContain newNoneOf ("HI"))
        all (hiLists) should (not be (List("hi")) or not newContain newNoneOf ("HI"))
        all (hiLists) should (not be (List("ho")) or not newContain newNoneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) or not newContain newNoneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) or not newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("hi")) or not newContain newNoneOf ("HI"))) (decided by upperCaseStringEquality)
        (all (hiLists) should (not be (List("ho")) or not newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) or not newContain newNoneOf ("hi"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", and " + decorateToStringValue(List("hi")) + " did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
