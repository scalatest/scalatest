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
import org.scalautils.StringNormalizations._
import SharedHelpers._

class ListShouldContainOneOfLogicalAndSpec extends Spec with Matchers {
  
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
  
  val fileName: String = "ListShouldContainOneOfLogicalAndSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain oneOf (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("happy", "birthday", "to", "you") and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("FIE", "FEE", "FUM", "FOE"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and (newContain newOneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("FIE", "FEE", "FUM", "FOE"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) and newContain newOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, fumList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (equal (fumList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (equal (fumList) and (newContain newOneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, fumList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (equal (toList) and newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, toList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) and newContain newOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and newContain newOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, fumList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (legacyEqual (fumList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (legacyEqual (fumList) and (newContain newOneOf ("fee", "fie", "foe", "fum")))
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, fumList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (fumList) and newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", fumList, fumList) + ", but " + Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) and newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain oneOf (...) and legacyEqual (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain newOneOf("fie", "fee", "fum", "foe") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fee", "fie", "foe", "fum") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and legacyEqual (fumList))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and legacyEqual (fumList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fie", "fee", "fum", "foe") and legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (newContain newOneOf ("fie", "fee", "fum", "foe") and (legacyEqual (fumList)))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", fumList, "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain newOneOf ("fie", "fee", "fum", "foe") and legacyEqual (fumList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainOneOfElements", fumList, "\"fie\", \"fee\", \"fum\", \"foe\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and legacyEqual (toList))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain oneOf (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and not newContain newOneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\"") + ", but " + Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        
        val e2 = intercept[TestFailedException] {
          fumList should (not newContain newOneOf ("fee", "fie", "foe", "fum") and (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM") and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not newContain newOneOf ("fee", "fie", "foe", "fum") and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("didNotContainOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        (fumList should (newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM ") and newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) and not newContain newOneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", fumList, toList) + ", but " + Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not equal (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (toList) and (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("didNotEqual", fumList, toList) + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (fumList) and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, fumList) + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not equal (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by invertedListOfStringEquality, decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (not newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) and not newContain newOneOf("FEE", "FIE", "FOE", "FUM"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", fumList, toList) + ", but " + Resources("containedOneOfElements", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = upperCaseStringEquality
        fumList should (not be (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          fumList should (not be (toList) and (not newContain newOneOf ("FEE", "FIE", "FOE", "FUM")))
        }
        checkMessageStackDepth(e2, Resources("wasNotEqualTo", fumList, toList) + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (toList) and not newContain newOneOf ("FEE", "FIE", "FOE", "FUM"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasNotEqualTo", fumList, toList) + ", but " + Resources("containedOneOfElements", fumList, "\"FEE\", \"FIE\", \"FOE\", \"FUM\""), fileName, thisLineNumber - 2)
        val e2 = intercept[TestFailedException] {
          (fumList should (not be (fumList) and not newContain newOneOf ("fee", "fie", "foe", "fum"))) (decided by upperCaseStringEquality)
        }
        checkMessageStackDepth(e2, Resources("wasEqualTo", fumList, fumList), fileName, thisLineNumber - 2)
        (fumList should (not newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUU ") and not newContain newOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
    
    object `when used with (contain oneOf (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain newOneOf (3, 2, 1) and newContain newOneOf (1, 3, 4))
        atLeast (2, lists) should (newContain newOneOf (3, 1, 5) and newContain newOneOf (1, 3, 4))
        atMost (2, lists) should (newContain newOneOf (3, 2, 8) and newContain newOneOf (2, 3, 4))
        no (lists) should (newContain newOneOf (3, 6, 9) and newContain newOneOf (3, 4, 5))
        no (nils) should (newContain newOneOf (1, 2, 8) and newContain newOneOf (1, 3, 4))
        no (listsNil) should (newContain newOneOf(3, 8, 5) and newContain newOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain newOneOf (1, 6, 8) and newContain newOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) did not contain one of (1, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (newContain newOneOf (1, 2, 8) and newContain newOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, "List(2) contained one of (1, 2, 8), but List(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (newContain newOneOf ("hi", "hello") and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "List() did not contain one of (\"hi\", \"hello\")", thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (newContain newOneOf ("hi", "hello") and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "List(hi) contained one of (\"hi\", \"hello\"), but List(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (newContain newOneOf (1, 3, 4) and newContain newOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, "List() did not contain one of (1, 3, 4)", thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (lists) should (newContain newOneOf (1, 2, 8) and newContain newOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e6, allErrMsg(2, "List(2) contained one of (1, 2, 8), but List(2) did not contain one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (newContain newOneOf ("ho") and newContain newOneOf ("he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain newOneOf ("hi") and newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (newContain newOneOf ("ho") and newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) contained one of (\"ho\"), but List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain newOneOf ("ho") and newContain newOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newOneOf ("hi") and newContain newOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (newContain newOneOf ("ho") and newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) contained one of (\"ho\"), but List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1)) and newContain newOneOf (1, 3, 4))
        atLeast (2, lists) should (be (List(1)) and newContain newOneOf (1, 3, 4))
        atMost (2, lists) should (be (List(1)) and newContain newOneOf (2, 3, 4))
        no (lists) should (be (List(8)) and newContain newOneOf (3, 4, 5))
        no (nils) should (be (List(8)) and newContain newOneOf (1, 3, 4))
        no (listsNil) should (be (List(8)) and newContain newOneOf (3, 4, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (be (List(1)) and newContain newOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) was not equal to List(1)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and newContain newOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(1) was equal to List(1), but List(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (nils) should (be (List("hey")) and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "List() was not equal to List(hey)", thisLineNumber - 2, nils), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "List(hi) was equal to List(hi), but List(hi) did not contain one of (\"ho\", \"hey\", \"howdy\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e5 = intercept[TestFailedException] {
          all (listsNil) should (be (List(1)) and newContain newOneOf (1, 3, Nil))
        }
        checkMessageStackDepth(e5, allErrMsg(2, "List() was not equal to List(1)", thisLineNumber - 2, listsNil), fileName, thisLineNumber - 2)
        
        val e6 = intercept[TestFailedException] {
          all (list1s) should (be (List(1)) and newContain newOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e6, allErrMsg(0, "List(1) was equal to List(1), but List(1) did not contain one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (be (List("hi")) and newContain newOneOf ("he"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) and newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was not equal to List(ho)", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (be (List("hi")) and newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) was equal to List(hi), but List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi")) and newContain newOneOf ("ho"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) and newContain newOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was not equal to List(ho)", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("hi")) and newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) was equal to List(hi), but List(hi) did not contain one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain oneOf (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain newOneOf (3, 2, 8) and not newContain newOneOf (8, 3, 4))
        atLeast (2, lists) should (not newContain newOneOf (3, 8, 5) and not newContain newOneOf (8, 3, 4))
        atMost (2, lists) should (not newContain newOneOf (3, 6, 8) and newContain newOneOf (5, 3, 4))
        no (lists) should (not newContain newOneOf (1, 2, 9) and not newContain newOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain newOneOf (2, 6, 8) and not newContain newOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) contained one of (2, 6, 8)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not newContain newOneOf (3, 6, 8) and not newContain newOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, "List(2) did not contain one of (3, 6, 8), but List(2) contained one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newOneOf ("hi", "hello") and not newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "List(hi) contained one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newOneOf ("ho", "hey", "howdy") and not newContain newOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "List(hi) did not contain one of (\"ho\", \"hey\", \"howdy\"), but List(hi) contained one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not newContain newOneOf ("hi") and not newContain newOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newOneOf ("ho") and not newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not newContain newOneOf ("hi") and not newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) did not contain one of (\"hi\"), but List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain newOneOf ("hi") and not newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newOneOf ("ho") and not newContain newOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain newOneOf ("hi") and not newContain newOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) did not contain one of (\"hi\"), but List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) and not newContain newOneOf (8, 3, 4))
        atLeast (2, lists) should (not be (List(3)) and not newContain newOneOf (8, 3, 4))
        atMost (2, lists) should (not be (List(3)) and newContain newOneOf (5, 3, 4))
        no (list1s) should (not be (List(1)) and not newContain newOneOf (2, 1, 5))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not be (List(2)) and not newContain newOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) was equal to List(2)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (lists) should (not be (List(3)) and not newContain newOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e2, allErrMsg(2, "List(2) was not equal to List(3), but List(2) contained one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
        
        val e3 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not newContain newOneOf ("ho", "hey", "howdy"))
        }
        checkMessageStackDepth(e3, allErrMsg(0, "List(hi) was equal to List(hi)", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e4 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newOneOf ("hi", "hello"))
        }
        checkMessageStackDepth(e4, allErrMsg(0, "List(hi) was not equal to List(ho), but List(hi) contained one of (\"hi\", \"hello\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not be (List("ho")) and not newContain newOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) and not newContain newOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was equal to List(hi)", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("ho")) and not newContain newOneOf ("ho"))
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) was not equal to List(ho), but List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) and not newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) and not newContain newOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was equal to List(hi)", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
        
        val e2 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("ho")) and not newContain newOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e2, allErrMsg(0, "List(hi) was not equal to List(ho), but List(hi) contained one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
