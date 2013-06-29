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
import org.scalautils.OpenNormalization
import org.scalautils.StringNormalizations._
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ListShouldContainAtLeastOneOfLogicalAndSpec extends Spec with Matchers {
  
  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val invertedListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
    }
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldContainAtLeastOneOfLogicalAndSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain oneOf (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and contain atLeastOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "fum", "fum", "fum") and contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e2, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "fum", "fum") and contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (equal (fumList) and contain atLeastOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (toList) and contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) and contain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (legacyEqual (fumList) and contain atLeastOneOf ("have", "a", "nice", "day"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and (contain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) and contain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (fumList) and contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) and contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain oneOf (..) and legacyEqual (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (contain atLeastOneOf("fie", "fee", "fum", "foe") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fee", "fie", "foe", "fum") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and legacyEqual (fumList))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (contain atLeastOneOf ("have", "a", "nice", "day") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "fum", "fum") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (contain atLeastOneOf ("fum", "fum", "fum") and (legacyEqual (fumList)))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (contain atLeastOneOf ("have", "a", "nice", "day") and legacyEqual (fumList))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("fum", "fum", "fum") and legacyEqual (fumList))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (contain atLeastOneOf ("happy", "birthday", "to", "you") and legacyEqual (toList))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fuu") and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("fee", "fie", "foe", "fum") and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\"") + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") and not contain atLeastOneOf ("fum", "fum", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") and not contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not contain atLeastOneOf ("fum", "fum", "fum") and (not contain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\"") + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not contain atLeastOneOf ("fum", "fum", "fum", "fum") and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("fum", "fum", "fum") and not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", decorateToStringValue(fumList), "\"fum\", \"fum\", \"fum\"") + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not contain atLeastOneOf ("happy", "birthday", "to", "you") and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        (fumList should (contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not equal (toList) and not contain atLeastOneOf ("fum", "fum", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not contain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", decorateToStringValue(fumList), decorateToStringValue(fumList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("equaled", decorateToStringValue(fumList), decorateToStringValue(toList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) and not contain atLeastOneOf("fie", "fee", "fuu", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not contain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not be (toList) and not contain atLeastOneOf ("fum", "fum", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not contain atLeastOneOf ("fum", "fum", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not contain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not contain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", decorateToStringValue(fumList), decorateToStringValue(toList)) + ", but " + Resources("containedAtLeastOneOf", decorateToStringValue(fumList), "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not contain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", decorateToStringValue(fumList), decorateToStringValue(fumList)), fileName, thisLineNumber - 2)
        (fumList should (not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not contain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
        all (list1s) should (contain atLeastOneOf (3, 2, 1) and contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (contain atLeastOneOf (3, 1, 5) and contain atLeastOneOf (1, 3, 4))
        atMost (2, lists) should (contain atLeastOneOf (3, 2, 8) and contain atLeastOneOf (2, 3, 4))
        no (lists) should (contain atLeastOneOf (3, 6, 9) and contain atLeastOneOf (3, 4, 5))
        no (nils) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        no (listsNil) should (contain atLeastOneOf(3, 8, 5) and contain atLeastOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 6, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " did not contain at least one of (1, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(2)) + " contained at least one of (1, 2, 8), but " + decorateToStringValue(List(2)) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (contain atLeastOneOf ("hi", "hello") and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Nil) + " did not contain at least one of (\"hi\", \"hello\")", thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi", "hello") and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"hi\", \"hello\"), but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (contain atLeastOneOf (1, 3, 4) and contain atLeastOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(Nil) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (lists) should (contain atLeastOneOf (1, 2, 8) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e6, allErrMsg(2, decorateToStringValue(List(2)) + " contained at least one of (1, 2, 8), but " + decorateToStringValue(List(2)) + " did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (contain atLeastOneOf ("ho") and contain atLeastOneOf ("he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("hi") and contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (contain atLeastOneOf ("ho") and contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\"), but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (contain atLeastOneOf ("ho") and contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("hi") and contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (contain atLeastOneOf ("ho") and contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\"), but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (..) and contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        atMost (2, lists) should (be (List(1)) and contain atLeastOneOf (2, 3, 4))
        no (lists) should (be (List(8)) and contain atLeastOneOf (3, 4, 5))
        no (nils) should (be (List(8)) and contain atLeastOneOf (1, 3, 4))
        no (listsNil) should (be (List(8)) and contain atLeastOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(1)) and contain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " was not equal to " + decorateToStringValue(List(1)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and contain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", but " + decorateToStringValue(List(1)) + " did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List("hey")), thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(1)) and contain atLeastOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, decorateToStringValue(Nil) + " was not equal to " + decorateToStringValue(List(1)), thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and contain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, decorateToStringValue(List(1)) + " was equal to " + decorateToStringValue(List(1)) + ", but " + decorateToStringValue(List(1)) + " did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) and contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) and contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi")) and contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")) + ", but " + decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not contain atLeastOneOf (3, 2, 8) and not contain atLeastOneOf (8, 3, 4))
        atLeast (2, lists) should (not contain atLeastOneOf (3, 8, 5) and not contain atLeastOneOf (8, 3, 4))
        atMost (2, lists) should (not contain atLeastOneOf (3, 6, 8) and contain atLeastOneOf (5, 3, 4))
        no (lists) should (not contain atLeastOneOf (1, 2, 9) and not contain atLeastOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneOf (2, 6, 8) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " contained at least one of (2, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not contain atLeastOneOf (3, 6, 8) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(2)) + " did not contain at least one of (3, 6, 8), but " + decorateToStringValue(List(2)) + " contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("hi", "hello") and not contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("ho", "hey", "howdy") and not contain atLeastOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"ho\", \"hey\", \"howdy\"), but " + decorateToStringValue(List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not contain atLeastOneOf ("hi") and not contain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("ho") and not contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not contain atLeastOneOf ("hi") and not contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\"), but " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not contain atLeastOneOf ("hi") and not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("ho") and not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not contain atLeastOneOf ("hi") and not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " did not contain at least one of (\"hi\"), but " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) and not contain atLeastOneOf (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not contain atLeastOneOf (8, 3, 4))
        atMost (2, lists) should (not be (List(3)) and contain atLeastOneOf (5, 3, 4))
        no (list1s) should (not be (List(1)) and not contain atLeastOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(2)) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, decorateToStringValue(List(2)) + " was equal to " + decorateToStringValue(List(2)), thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not contain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, decorateToStringValue(List(2)) + " was not equal to " + decorateToStringValue(List(3)) + ", but " + decorateToStringValue(List(2)) + " contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi")) + " contained at least one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) and not contain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, decorateToStringValue(List("hi")) + " was equal to " + decorateToStringValue(List("hi")), thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not contain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, decorateToStringValue(List("hi")) + " was not equal to " + decorateToStringValue(List("ho")) + ", but " + decorateToStringValue(List("hi")) + " contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
