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

class ListShouldContainAtLeastOneOfLogicalOrSpec extends Spec with Matchers {
  
  val invertedStringEquality =
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }
  
  val invertedListOfStringEquality = 
    new Equality[List[String]] {
      def areEqual(a: List[String], b: Any): Boolean = a != b
    }
  
  val fileName: String = "ListShouldContainAtLeastOneOfLogicalOrSpec.scala"
  
  object `a List` {
    
    val fumList: List[String] = List("fum")
    val toList: List[String] = List("to")
    
    object `when used with (contain oneOf (...) or contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain atLeastOneOf ("fee", "fie", "foe", "fum") or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (newContain atLeastOneOf ("fee", "fie", "foe", "fam") or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (newContain atLeastOneOf ("fee", "fie", "foe", "fum") or newContain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain atLeastOneOf ("fee", "fie", "foe", "fam") or newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }

      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (newContain atLeastOneOf ("happy", "birthday", "to", "you") or newContain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (newContain atLeastOneOf ("happy", "birthday", "to", "you") or newContain atLeastOneOf ("fie", "fee", "fum", "foe"))
        fumList should (newContain atLeastOneOf ("fie", "fee", "fum", "foe") or newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain atLeastOneOf ("fum", "fum", "fum", "fum") or (newContain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain atLeastOneOf ("happy", "birthday", "to", "you") or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (newContain atLeastOneOf ("happy", "birthday", "to", "you") or newContain atLeastOneOf ("fie", "fee", "fum", "foe"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (newContain atLeastOneOf ("fie", "fee", "fum", "foe") or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain atLeastOneOf ("fum", "fum") or newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\"") + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (equal (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (equal (fumList) or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (toList) or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (equal (fumList) or newContain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (equal (fumList) or newContain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (equal (toList) or newContain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (equal (fumList) or newContain atLeastOneOf ("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (equal (toList) or (newContain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (equal (toList) or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (equal (fumList) or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (equal (toList) or newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (equal (fumList) or newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, fumList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (equal (toList) or newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (decided by invertedListOfStringEquality, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (legacyEqual (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (legacyEqual (fumList) or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (toList) or newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        fumList should (legacyEqual (fumList) or newContain atLeastOneOf("fie", "fee", "fam", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (legacyEqual (fumList) or newContain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (legacyEqual (toList) or newContain atLeastOneOf ("have", "a", "nice", "day"))
        fumList should (legacyEqual (fumList) or newContain atLeastOneOf ("fum", "fum", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (legacyEqual (toList) or (newContain atLeastOneOf ("fum", "fum", "fum")))
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (legacyEqual (fumList) or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumList should (legacyEqual (toList) or newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality)
        (fumList should (legacyEqual (fumList) or newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (legacyEqual (toList) or newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotEqual", fumList, toList) + ", and " + Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\""), fileName, thisLineNumber - 2)
        (fumList should (legacyEqual (fumList) or newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed)
      }
    }

    object `when used with (contain oneOf (...) and legacyEqual (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (newContain atLeastOneOf("fie", "fee", "fum", "foe") or legacyEqual (fumList))
        fumList should (newContain atLeastOneOf("fie", "fee", "fam", "foe") or legacyEqual (fumList))
        fumList should (newContain atLeastOneOf("fie", "fee", "fum", "foe") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain atLeastOneOf ("fee", "fie", "foe", "fam") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fam\"") + ", and " + Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (newContain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (fumList))
        fumList should (newContain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (fumList))
        fumList should (newContain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (toList))
        val e1 = intercept[TestFailedException] {
          fumList should (newContain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (toList))
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (newContain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (fumList))) (decided by invertedStringEquality)
        (fumList should (newContain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (fumList))) (decided by invertedStringEquality)
        (fumList should (newContain atLeastOneOf ("have", "a", "nice", "day") or legacyEqual (toList))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (newContain atLeastOneOf ("fum", "fum", "fum") or legacyEqual (toList))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("didNotContainAtLeastOneOf", fumList, "\"fum\", \"fum\", \"fum\"") + ", and " + Resources("didNotEqual", fumList, toList), fileName, thisLineNumber - 2)
        (fumList should (newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or legacyEqual (fumList))) (after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not contain oneOf (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not newContain atLeastOneOf ("fee", "fie", "foe", "fuu") or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not newContain atLeastOneOf ("fee", "fie", "foe", "fum") or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not newContain atLeastOneOf ("fee", "fie", "foe", "fuu") or not newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain atLeastOneOf ("fee", "fie", "foe", "fum") or not newContain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fum\"") + ", and " + Resources("containedAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not newContain atLeastOneOf ("fum", "fum", "fum", "fum") or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not newContain atLeastOneOf ("happy", "birthday", "to", "you") or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not newContain atLeastOneOf ("fum", "fum", "fum", "fum") or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (not newContain atLeastOneOf ("happy", "birthday", "to", "you") or not newContain atLeastOneOf ("have", "a", "nice", "day"))
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\"") + ", and " + Resources("containedAtLeastOneOf", fumList, "\"have\", \"a\", \"nice\", \"day\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not newContain atLeastOneOf ("fum", "fum", "fum", "fum") or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (not newContain atLeastOneOf ("happy", "birthday", "to", "you") or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (fumList should (not newContain atLeastOneOf ("fum", "fum", "fum", "fum") or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not newContain atLeastOneOf ("happy", "birthday", "to", "you") or not newContain atLeastOneOf ("have", "a", "nice", "day"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\"") + ", and " + Resources("containedAtLeastOneOf", fumList, "\"have\", \"a\", \"nice\", \"day\""), fileName, thisLineNumber - 2)
        (fumList should (newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM ") or newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUM "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not equal (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not equal (toList) or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not equal (fumList) or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not equal (toList) or not newContain atLeastOneOf("fie", "fee", "fum", "foe"))
        val e1 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or not newContain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("equaled", fumList, fumList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not equal (toList) or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not equal (fumList) or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not equal (toList) or not newContain atLeastOneOf ("fie", "fee", "fuu", "foe"))
        val e2 = intercept[TestFailedException] {
          fumList should (not equal (fumList) or (not newContain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e2, Resources("equaled", fumList, fumList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not equal (fumList) or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (not equal (toList) or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        (fumList should (not equal (fumList) or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not equal (toList) or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedListOfStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("equaled", fumList, toList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        fumList should (not be (toList) or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not be (fumList) or not newContain atLeastOneOf("fie", "fee", "fuu", "foe"))
        fumList should (not be (toList) or not newContain atLeastOneOf("fee", "fie", "foe", "fum"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or not newContain atLeastOneOf ("fee", "fie", "foe", "fum"))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", fumList, fumList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"fee\", \"fie\", \"foe\", \"fum\""), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        fumList should (not be (toList) or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not be (fumList) or not newContain atLeastOneOf ("fum", "fum", "fum"))
        fumList should (not be (toList) or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))
        val e1 = intercept[TestFailedException] {
          fumList should (not be (fumList) or (not newContain atLeastOneOf ("happy", "birthday", "to", "you")))
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", fumList, fumList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (fumList should (not be (toList) or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        (fumList should (not be (fumList) or not newContain atLeastOneOf ("fum", "fum", "fum"))) (decided by invertedStringEquality)
        (fumList should (not be (toList) or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        val e1 = intercept[TestFailedException] {
          (fumList should (not be (fumList) or not newContain atLeastOneOf ("happy", "birthday", "to", "you"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, Resources("wasEqualTo", fumList, fumList) + ", and " + Resources("containedAtLeastOneOf", fumList, "\"happy\", \"birthday\", \"to\", \"you\""), fileName, thisLineNumber - 2)
        (fumList should (not newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU ") or not newContain atLeastOneOf (" FEE ", " FIE ", " FOE ", " FUU "))) (after being lowerCased and trimmed, after being lowerCased and trimmed)
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
    
    object `when used with (contain oneOf (..) and contain oneOf (..)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (newContain atLeastOneOf (3, 2, 1) or newContain atLeastOneOf (1, 3, 4))
        all (list1s) should (newContain atLeastOneOf (3, 2, 5) or newContain atLeastOneOf (1, 3, 4))
        all (list1s) should (newContain atLeastOneOf (3, 2, 1) or newContain atLeastOneOf (2, 3, 4))
        
        atLeast (2, lists) should (newContain atLeastOneOf (3, 1, 5) or newContain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (newContain atLeastOneOf (3, 6, 5) or newContain atLeastOneOf (1, 3, 4))
        atLeast (2, lists) should (newContain atLeastOneOf (3, 1, 5) or newContain atLeastOneOf (8, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (newContain atLeastOneOf (6, 7, 8) or newContain atLeastOneOf (1, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) did not contain at least one of (6, 7, 8), and List(2) did not contain at least one of (1, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (newContain atLeastOneOf ("ho") or newContain atLeastOneOf ("he"))
        all (hiLists) should (newContain atLeastOneOf ("hi") or newContain atLeastOneOf ("he"))
        all (hiLists) should (newContain atLeastOneOf ("ho") or newContain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (newContain atLeastOneOf ("hi") or newContain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) did not contain at least one of (\"hi\"), and List(hi) did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (newContain atLeastOneOf ("ho") or newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (newContain atLeastOneOf ("hi") or newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (newContain atLeastOneOf ("ho") or newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (newContain atLeastOneOf ("hi") or newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) did not contain at least one of (\"hi\"), and List(hi) did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (be (...) and contain oneOf (...)) syntax` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (be (List(1)) or newContain atLeastOneOf (1, 3, 4))
        all (list1s) should (be (List(2)) or newContain atLeastOneOf (1, 3, 4))
        all (list1s) should (be (List(1)) or newContain atLeastOneOf (2, 3, 4))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (be (List(2)) or newContain atLeastOneOf (2, 3, 8))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(1) was not equal to List(2), and List(1) did not contain at least one of (2, 3, 8)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (be (List("hi")) or newContain atLeastOneOf ("he"))
        all (hiLists) should (be (List("ho")) or newContain atLeastOneOf ("he"))
        all (hiLists) should (be (List("hi")) or newContain atLeastOneOf ("hi"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (be (List("ho")) or newContain atLeastOneOf ("hi"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was not equal to List(ho), and List(hi) did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (be (List("hi")) or newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        (all (hiLists) should (be (List("ho")) or newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        (all (hiLists) should (be (List("hi")) or newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (be (List("ho")) or newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was not equal to List(ho), and List(hi) did not contain at least one of (\"hi\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not contain oneOf (..) and not contain oneOf (..))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not newContain atLeastOneOf (3, 2, 8) or not newContain atLeastOneOf (8, 3, 4))
        all (list1s) should (not newContain atLeastOneOf (1, 2, 8) or not newContain atLeastOneOf (8, 3, 4))
        all (list1s) should (not newContain atLeastOneOf (3, 2, 8) or not newContain atLeastOneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (lists) should (not newContain atLeastOneOf (2, 6, 8) or not newContain atLeastOneOf (2, 3, 4))
        }
        checkMessageStackDepth(e1, allErrMsg(2, "List(2) contained at least one of (2, 6, 8), and List(2) contained at least one of (2, 3, 4)", thisLineNumber - 2, lists), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not newContain atLeastOneOf ("hi") or not newContain atLeastOneOf ("hi"))
        all (hiLists) should (not newContain atLeastOneOf ("ho") or not newContain atLeastOneOf ("hi"))
        all (hiLists) should (not newContain atLeastOneOf ("hi") or not newContain atLeastOneOf ("h0"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not newContain atLeastOneOf ("ho") or not newContain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) contained at least one of (\"ho\"), and List(hi) contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not newContain atLeastOneOf ("hi") or not newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (not newContain atLeastOneOf ("ho") or not newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        (all (hiLists) should (not newContain atLeastOneOf ("hi") or not newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not newContain atLeastOneOf ("ho") or not newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality, decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) contained at least one of (\"ho\"), and List(hi) contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
    
    object `when used with (not be (...) and not contain oneOf (...))` {
      
      def `should do nothing if valid, else throw a TFE with an appropriate error message` {
        all (list1s) should (not be (List(2)) or not newContain atLeastOneOf (8, 3, 4))
        all (list1s) should (not be (List(1)) or not newContain atLeastOneOf (8, 3, 4))
        all (list1s) should (not be (List(2)) or not newContain atLeastOneOf (8, 3, 1))
        
        val e1 = intercept[TestFailedException] {
          all (list1s) should (not be (List(1)) or not newContain atLeastOneOf (2, 3, 1))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(1) was equal to List(1), and List(1) contained at least one of (2, 3, 1)", thisLineNumber - 2, list1s), fileName, thisLineNumber - 2)
      }
      
      def `should use the implicit Equality in scope` {
        implicit val ise = invertedStringEquality
        
        all (hiLists) should (not be (List("ho")) or not newContain atLeastOneOf ("hi"))
        all (hiLists) should (not be (List("hi")) or not newContain atLeastOneOf ("hi"))
        all (hiLists) should (not be (List("ho")) or not newContain atLeastOneOf ("ho"))
        
        val e1 = intercept[TestFailedException] {
          all (hiLists) should (not be (List("hi")) or not newContain atLeastOneOf ("ho"))
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was equal to List(hi), and List(hi) contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
      
      def `should use an explicitly provided Equality` {
        (all (hiLists) should (not be (List("ho")) or not newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        (all (hiLists) should (not be (List("hi")) or not newContain atLeastOneOf ("hi"))) (decided by invertedStringEquality)
        (all (hiLists) should (not be (List("ho")) or not newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        
        val e1 = intercept[TestFailedException] {
          (all (hiLists) should (not be (List("hi")) or not newContain atLeastOneOf ("ho"))) (decided by invertedStringEquality)
        }
        checkMessageStackDepth(e1, allErrMsg(0, "List(hi) was equal to List(hi), and List(hi) contained at least one of (\"ho\")", thisLineNumber - 2, hiLists), fileName, thisLineNumber - 2)
      }
    }
  }
}
