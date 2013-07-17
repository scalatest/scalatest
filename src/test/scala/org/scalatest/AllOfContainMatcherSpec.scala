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

class AllOfContainMatcherSpec extends Spec with Matchers {

  object `allOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(5, 4, 3, 2, 1) should contain allOf(1, 3, 5)
      Array(5, 4, 3, 2, 1) should contain allOf(1, 3, 5)
      javaList(5, 4, 3, 2, 1) should contain allOf(1, 3, 5)
      
      Map(5 -> "five", 4 -> "four", 3 -> "three", 2 -> "two", 1 -> "one") should contain allOf (1 -> "one", 3 -> "three", 5 -> "five")
      javaMap(Entry(5, "five"), Entry(4, "four"), Entry(3, "three"), Entry(2, "two"), Entry(1, "one")) should contain allOf (Entry(1, "one"), Entry(3, "three"), Entry(5, "five"))
    }
    
    def `should succeeded when left List contains same elements in different order as right List` {
      List(1, 2, 3) should contain allOf(2, 1, 3)
      Array(1, 2, 3) should contain allOf(2, 1, 3)
      javaList(1, 2, 3) should contain allOf(2, 1, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (2 -> "two", 1 -> "one", 3 -> "three")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain allOf (Entry(2, "two"), Entry(1, "one"), Entry(3, "three"))
    }
    
    def `should succeeded when left List contains same elements in same order as right Set` {
      List(1, 2, 3) should contain allOf(1, 2, 3)
      Array(1, 2, 3) should contain allOf(1, 2, 3)
      javaList(1, 2, 3) should contain allOf(1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain allOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
    }
    
    def `should throw NotAllowedException when allOf contains duplicate element` {
      val e1 = intercept[exceptions.NotAllowedException] {
        List(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e1.getMessage() should be ("allOf must not contain duplicated value")
      
      val e2 = intercept[exceptions.NotAllowedException] {
        javaList(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e2.getMessage() should be ("allOf must not contain duplicated value")
      
      val e3 = intercept[exceptions.NotAllowedException] {
        Array(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e3.getMessage() should be ("allOf must not contain duplicated value")
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allOf (2, 5, 3)
      }
      checkStackDepth(e1, left1, Array(2, 5, 3).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allOf (2, 5, 3)
      }
      checkStackDepth(e2, left2, Array(2, 5, 3).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allOf (2 -> "two", 5 -> "five", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(2 -> "two", 5 -> "five", 3 -> "three"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (Entry(2, "two"), Entry(5, "five"), Entry(3, "three"))
      }
      checkStackDepth(e4, left4, Array(Entry(2, "two"), Entry(5, "five"), Entry(3, "three")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allOf (2, 5, 3)
      }
      checkStackDepth(e5, left5, Array(2, 5, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allOf (1, 2, 3, 4)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3, 4).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allOf (1, 2, 3, 4)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3, 4).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"))
      }
      checkStackDepth(e4, left4, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allOf (1, 2, 3, 4)
      }
      checkStackDepth(e5, left5, Array(1, 2, 3, 4).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allOf (1, 5)
      }
      checkStackDepth(e1, left1, Array(1, 5).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allOf (1, 5)
      }
      checkStackDepth(e2, left2, Array(1, 5).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allOf (1 -> "one", 5 -> "five")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 5 -> "five"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (Entry(1, "one"), Entry(5, "five"))
      }
      checkStackDepth(e4, left4, Array(Entry(1, "one"), Entry(5, "five")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allOf (1, 5)
      }
      checkStackDepth(e5, left5, Array(1, 5).deep, thisLineNumber - 2)
    }
  }
  
  object `not allOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained all of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("AllOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains different elements as right List` {
      List(1, 2, 3) should not contain allOf (1, 2, 8)
      Array(1, 2, 3) should not contain allOf (1, 2, 8)
      javaList(1, 2, 3) should not contain allOf (1, 2, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain allOf (1 -> "one", 2 -> "two", 8 -> "eight")
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain allOf (Entry(1, "one"), Entry(2, "two"), Entry(8, "eight"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain same elements in different order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allOf (2, 1, 3)
      }
      checkStackDepth(e1, left1, Array(2, 1, 3).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf (2, 1, 3)
      }
      checkStackDepth(e2, left2, Array(2, 1, 3).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allOf (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf (Entry(2, "two"), Entry(1, "one"), Entry(3, "three"))
      }
      checkStackDepth(e4, left4, Array(Entry(2, "two"), Entry(1, "one"), Entry(3, "three")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (2, 1, 3)
      }
      checkStackDepth(e5, left5, Array(2, 1, 3).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain same elements in same order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      }
      checkStackDepth(e4, left4, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e5, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
