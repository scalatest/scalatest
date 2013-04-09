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
import words.AllOfContainMatcher

class AllOfContainMatcherSpec extends Spec with Matchers with SharedHelpers {

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
      javaMap(5 -> "five", 4 -> "four", 3 -> "three", 2 -> "two", 1 -> "one") should contain allOf (1 -> "one", 3 -> "three", 5 -> "five")
    }
    
    val matcher = new AllOfContainMatcher(List(1, 2), defaultEquality)
    val mapMatcherRight = Array(1 -> "one", 2 -> "two")
    val mapMatcher = new AllOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Array(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      
      javaList(1, 2, 3) should contain (matcher)
      javaSet(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (allOf(1, 2))
      Array(1, 2, 3) should contain (allOf(1, 2))
      Set(1, 2, 3) should contain (allOf(1, 2))
      
      javaList(1, 2, 3) should contain (allOf(1, 2))
      javaSet(1, 2, 3) should contain (allOf(1, 2))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (1 -> "one", 2 -> "two")
      
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (1 -> "one", 2 -> "two")
    }
    
    def `should succeeded when left List contains same elements in different order as right List` {
      List(1, 2, 3) should contain allOf(2, 1, 3)
      Array(1, 2, 3) should contain allOf(2, 1, 3)
      javaList(1, 2, 3) should contain allOf(2, 1, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (2 -> "two", 1 -> "one", 3 -> "three")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (2 -> "two", 1 -> "one", 3 -> "three")
    }
    
    def `should succeeded when left List contains same elements in same order as right Set` {
      List(1, 2, 3) should contain allOf(1, 2, 3)
      Array(1, 2, 3) should contain allOf(1, 2, 3)
      javaList(1, 2, 3) should contain allOf(1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw IllegalArgumentException when allOf contains duplicate element` {
      val e1 = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e1.getMessage() should be ("allOf must not contained duplicated value, but 1 is duplicated")
      
      val e2 = intercept[IllegalArgumentException] {
        javaList(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e2.getMessage() should be ("allOf must not contained duplicated value, but 1 is duplicated")
      
      val e3 = intercept[IllegalArgumentException] {
        Array(1, 2, 3) should contain allOf(1, 2, 1)
      }
      e3.getMessage() should be ("allOf must not contained duplicated value, but 1 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 3, 8)
      val left2 = Set(1, 3, 8)
      val left3 = Map(1 -> "one", 3 -> "three", 8 -> "eight")
      val left4 = javaList(1, 3, 8)
      val left5 = javaSet(1, 3, 8)
      val left6 = javaMap(1 -> "one", 3 -> "three", 8 -> "eight")
      val left7 = Array(1, 3, 8)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(1, 2).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(1, 2).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (allOf(1, 2))
      }
      checkStackDepth(e3, left1, Array(1, 2).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (allOf(1, 2))
      }
      checkStackDepth(e4, left2, Array(1, 2).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should contain (mapMatcher)
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should contain allOf (1 -> "one", 2 -> "two")
      }
      checkStackDepth(e6, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should contain (matcher)
      }
      checkStackDepth(e7, left4, Array(1, 2).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should contain (matcher)
      }
      checkStackDepth(e8, left5, Array(1, 2).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should contain (allOf(1, 2))
      }
      checkStackDepth(e9, left4, Array(1, 2).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should contain (allOf(1, 2))
      }
      checkStackDepth(e10, left5, Array(1, 2).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should contain (mapMatcher)
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should contain allOf (1 -> "one", 2 -> "two")
      }
      checkStackDepth(e12, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should contain (matcher)
      }
      checkStackDepth(e13, left7, Array(1, 2).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should contain (allOf(1, 2))
      }
      checkStackDepth(e14, left7, Array(1, 2).deep, thisLineNumber - 2)
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (2 -> "two", 5 -> "five", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(2 -> "two", 5 -> "five", 3 -> "three"), thisLineNumber - 2)
      
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"), thisLineNumber - 2)
      
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allOf (1 -> "one", 5 -> "five")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 5 -> "five"), thisLineNumber - 2)
      
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
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain allOf (1 -> "one", 2 -> "two", 8 -> "eight")
    }
    
    val matcher = new AllOfContainMatcher(List(1, 2, 3), defaultEquality)
    val mapMatcherRight = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val mapMatcher = new AllOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Array(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      javaList(1, 2, 8) should not contain matcher
      javaSet(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain allOf (1, 2, 3)
      Array(1, 2, 8) should not contain allOf (1, 2, 3)
      Set(1, 2, 8) should not contain allOf (1, 2, 3)
      
      javaList(1, 2, 8) should not contain allOf (1, 2, 3)
      javaSet(1, 2, 8) should not contain allOf (1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 3)
      val left2 = Set(1, 2, 3)
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val left4 = javaList(1, 2, 3)
      val left5 = javaSet(1, 2, 3)
      val left6 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val left7 = Array(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e3, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e4, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should not contain mapMatcher
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e6, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should not contain matcher
      }
      checkStackDepth(e7, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should not contain matcher
      }
      checkStackDepth(e8, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e9, left4, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e10, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should not contain mapMatcher
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e12, left6, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should not contain matcher
      }
      checkStackDepth(e13, left7, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e14, left7, Array(1, 2, 3).deep, thisLineNumber - 2)
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
      
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allOf (1, 2, 3)
      }
      checkStackDepth(e5, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
