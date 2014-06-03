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

import collection.GenTraversable
import SharedHelpers._
import Matchers._

class NoneOfContainMatcherSpec extends Spec {

  object `noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      e.message should be (Some(FailureMessages("containedOneOfElements", left, UnquotedString(right.mkString(", ")))))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains elements available in right List` {
      List(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
      Array(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
      javaList(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
      
      Set(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
      javaSet(1, 2, 3, 4, 5) should contain noneOf (6, 7, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"), Entry(5, "five")) should contain noneOf (Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"))
    }
    
    def `should succeed when left list contains none of right list` {
      List(1, 2, 3) should contain noneOf (7, 8)
      Array(1, 2, 3) should contain noneOf (7, 8)
      javaList(1, 2, 3) should contain noneOf (7, 8)
      
      Set(1, 2, 3) should contain noneOf (7, 8)
      javaSet(1, 2, 3) should contain noneOf (7, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain noneOf (7 -> "seven", 8 -> "eight")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain noneOf (Entry(7, "seven"), Entry(8, "eight"))
    }
    
    def `should throw NotAllowedException when noneOf contains duplicate element` {
      val e1 = intercept[exceptions.NotAllowedException] {
        List(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e1.getMessage() should be (FailureMessages("noneOfDuplicate"))
      
      val e2 = intercept[exceptions.NotAllowedException] {
        Set(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e2.getMessage() should be (FailureMessages("noneOfDuplicate"))
      
      val e3 = intercept[exceptions.NotAllowedException] {
        Array(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e3.getMessage() should be (FailureMessages("noneOfDuplicate"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains element in right List` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain noneOf (0, 3, 8)
      }
      checkStackDepth(e1, left1, Array(0, 3, 8).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain noneOf (0, 3, 8)
      }
      checkStackDepth(e2, left2, Array(0, 3, 8).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain noneOf (0 -> "zero", 3 -> "three", 8 -> "eight")
      }
      checkStackDepth(e3, left3, Array(0 -> "zero", 3 -> "three", 8 -> "eight"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain noneOf (Entry(0, "zero"), Entry(3, "three"), Entry(8, "eight"))
      }
      checkStackDepth(e4, left4, Array(Entry(0, "zero"), Entry(3, "three"), Entry(8, "eight")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain noneOf (0, 3, 8)
      }
      checkStackDepth(e5, left5, Array(0, 3, 8).deep, thisLineNumber - 2)
    }
  }
  
  object `not noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.mkString(", ")))))
      e.failedCodeFileName should be (Some("NoneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains element in right List` {
      List(1, 2, 3) should not contain noneOf (0, 2, 8)
      Array(1, 2, 3) should not contain noneOf (0, 2, 8)
      javaList(1, 2, 3) should not contain noneOf (0, 2, 8)
      
      Set(1, 2, 3) should not contain noneOf (0, 2, 8)
      javaSet(1, 2, 3) should not contain noneOf (0, 2, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain noneOf (Entry(0, "zero"), Entry(2, "two"), Entry(8, "eight"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (7, 8, 9)
      }
      checkStackDepth(e1, left1, Array(7, 8, 9).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (7, 8, 9)
      }
      checkStackDepth(e2, left2, Array(7, 8, 9).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e3, left3, Array(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
      }
      checkStackDepth(e4, left4, Array(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (7, 8, 9)
      }
      checkStackDepth(e5, left5, Array(7, 8, 9).deep, thisLineNumber - 2)
    }
  }
}
