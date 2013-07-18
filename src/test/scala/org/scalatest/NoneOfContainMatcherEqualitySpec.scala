/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import org.scalautils.Equality
import org.scalautils.Explicitly
import collection.GenTraversable
import SharedHelpers._

class NoneOfContainMatcherEqualitySpec extends Spec with Matchers with Explicitly {

  class TrimEquality extends Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }
  
  class MapTrimEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any) = 
      right match {
        case t2: Tuple2[_, _] =>  
          left._1 == t2._1 && 
          left._2.trim == (t2._2 match {
            case s: String => s.trim
            case other => other
          })
        case right => left == right
      }
  }
  
  class FalseEquality extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = false
  }
  
  class MapFalseEquality extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = false
  }
  
  class SetEquality(validLeft: Set[Int], validRight: Set[Any], returnValue: Boolean) extends Equality[Int] {
    def areEqual(left: Int, right: Any): Boolean = 
      if (validLeft.contains(left) && validRight.contains(right))
        returnValue
      else
        !returnValue
  }
  
  class MapSetEquality(validLeft: Set[(Int, String)], validRight: Set[Any], returnValue: Boolean) extends Equality[(Int, String)] {
    def areEqual(left: (Int, String), right: Any): Boolean = 
      if (validLeft.contains(left) && validRight.contains(right))
        returnValue
      else
        !returnValue
  }
  
  class JavaMapSetEquality(validLeft: Set[java.util.Map.Entry[Int, String]], validRight: Set[Any], returnValue: Boolean) extends Equality[java.util.Map.Entry[Int, String]] {
    def areEqual(left: java.util.Map.Entry[Int, String], right: Any): Boolean = 
      if (validLeft.contains(left) && validRight.contains(right))
        returnValue
      else
        !returnValue
  }
  
  object `noneOf ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained one of (" + right.map(FailureMessages.decorateToStringValue).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain one of (" + right.map(FailureMessages.decorateToStringValue).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should contain noneOf (1, 2, 3)
      Set(1, 2, 3) should contain noneOf (1, 2, 3)
      Array(1, 2, 3) should contain noneOf (1, 2, 3)
      javaList(1, 2, 3) should contain noneOf (1, 2, 3)
      javaSet(1, 2, 3) should contain noneOf (1, 2, 3)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> " two", 3 -> "three"), Set(1 -> "one", 2 -> " two", 3 -> "three"), false)
      Map(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")
      
      implicit val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")), Set(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")), false)
      javaMap(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")) should contain noneOf (Entry(1, "one"), Entry(2, " two"), Entry(3, "three"))
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(7, 8, 9), true)
      List(1, 2, 3) should not contain noneOf (7, 8, 9)
      Set(1, 2, 3) should not contain noneOf (7, 8, 9)
      Array(1, 2, 3) should not contain noneOf (7, 8, 9)
      javaList(1, 2, 3) should not contain noneOf (7, 8, 9)
      javaSet(1, 2, 3) should not contain noneOf (7, 8, 9)
      
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(7 -> "seven", 8 -> "eight", 9 -> "nine"), true)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      
      implicit val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), true)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain noneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(6, 7, 8), true)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain noneOf (6, 7, 8)
      }
        checkShouldContainStackDepth(e3, left3, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain noneOf (6, 7, 8)
      }
      checkShouldContainStackDepth(e4, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(6 -> "six", 7 -> "seven", 8 -> "eight"), true)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      }
      checkShouldContainStackDepth(e5, left5, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
      
      implicit val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight")), true)
      
      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should contain noneOf (Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"))
      }
      checkShouldContainStackDepth(e6, left6, Array(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight")).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new SetEquality(Set(1, 2, 3), Set(1, 2, 3), false)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (1, 2, 3)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      implicit val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(1 -> "one", 2 -> "two", 3 -> "three"), false)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
      
      implicit val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), false)
      
      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        left6 should not contain noneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      }
      checkShouldNotContainStackDepth(e6, left6, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")).deep, thisLineNumber - 2)
    }
    
    def `should take custom explicit equality in scope when 'should contain' is used` {
      val equality = new FalseEquality
      (List(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (Set(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (Array(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (javaList(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
      (javaSet(1, 2, 3) should contain noneOf (1, 2, 3)) (equality)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> " two", 3 -> "three"), Set(1 -> "one", 2 -> " two", 3 -> "three"), false)
      (Map(1 -> "one", 2 -> " two", 3 -> "three") should contain noneOf (1 -> "one", 2 -> " two", 3 -> "three")) (mapEquality)
      
      val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")), Set(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")), false)
      (javaMap(Entry(1, "one"), Entry(2, " two"), Entry(3, "three")) should contain noneOf (Entry(1, "one"), Entry(2, " two"), Entry(3, "three"))) (javaMapEquality)
    }
    
    def `should take custom explicit equality in scope when 'should not contain' is used` {
      val equality = new SetEquality(Set(1, 2, 3), Set(7, 8, 9), true)
      (List(1, 2, 3) should not contain noneOf (7, 8, 9)) (equality)
      (Set(1, 2, 3) should not contain noneOf (7, 8, 9)) (equality)
      (Array(1, 2, 3) should not contain noneOf (7, 8, 9)) (equality)
      (javaList(1, 2, 3) should not contain noneOf (7, 8, 9)) (equality)
      (javaSet(1, 2, 3) should not contain noneOf (7, 8, 9)) (equality)
      
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(7 -> "seven", 8 -> "eight", 9 -> "nine"), true)
      (Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")) (mapEquality)
      
      val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), true)
      (javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain noneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))) (javaMapEquality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality in scope` {
      val equality = new SetEquality(Set(1, 2, 3), Set(6, 7, 8), true)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain noneOf (6, 7, 8)) (equality)
      }
        checkShouldContainStackDepth(e3, left3, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should contain noneOf (6, 7, 8)) (equality)
      }
      checkShouldContainStackDepth(e4, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(6 -> "six", 7 -> "seven", 8 -> "eight"), true)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")) (mapEquality)
      }
      checkShouldContainStackDepth(e5, left5, Array(6 -> "six", 7 -> "seven", 8 -> "eight").deep, thisLineNumber - 2)
      
      val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight")), true)
      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should contain noneOf (Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"))) (javaMapEquality)
      }
      checkShouldContainStackDepth(e6, left6, Array(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight")).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality in scope` {
      val equality = new SetEquality(Set(1, 2, 3), Set(1, 2, 3), false)
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain noneOf (1, 2, 3)) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left2 = Set(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain noneOf (1, 2, 3)) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain noneOf (1, 2, 3)) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        (left4 should not contain noneOf (1, 2, 3)) (equality)
      }
      checkShouldNotContainStackDepth(e4, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
        
      val mapEquality = new MapSetEquality(Set(1 -> "one", 2 -> "two", 3 -> "three"), Set(1 -> "one", 2 -> "two", 3 -> "three"), false)
        
      val left5 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e5 = intercept[exceptions.TestFailedException] {
        (left5 should not contain noneOf (1 -> "one", 2 -> "two", 3 -> "three")) (mapEquality)
      }
      checkShouldNotContainStackDepth(e5, left5, Array(1 -> "one", 2 -> "two", 3 -> "three").deep, thisLineNumber - 2)
      
      val javaMapEquality = new JavaMapSetEquality(Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), Set(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), false)
      
      val left6 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e6 = intercept[exceptions.TestFailedException] {
        (left6 should not contain noneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) (javaMapEquality)
      }
      checkShouldNotContainStackDepth(e6, left6, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")).deep, thisLineNumber - 2)
    }
  }
}
