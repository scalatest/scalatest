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
import words.OneOfContainMatcher

class OneOfContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `oneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      Array(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      javaList(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      
      Set(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      javaSet(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      
      Map(1 -> "one", 3 -> "three", 5 -> "five", 8 -> "eight") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      javaMap(1 -> "one", 3 -> "three", 5 -> "five", 8 -> "eight") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
    }
    
    val matcher = new OneOfContainMatcher(List(5, 3, 8), defaultEquality)
    val mapMatcherRight = Map(5 -> "five", 3 -> "three", 8 -> "eight")
    val mapMatcher = new OneOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3) should contain (matcher)
      Set(1, 2, 3) should contain (matcher)
      Array(1, 2, 3) should contain (matcher)
      
      javaList(1, 2, 3) should contain (matcher)
      javaSet(1, 2, 3) should contain (matcher)
      
      List(1, 2, 3) should contain (oneOf(5, 3, 8))
      Set(1, 2, 3) should contain (oneOf(5, 3, 8))
      Array(1, 2, 3) should contain (oneOf(5, 3, 8))
      
      javaList(1, 2, 3) should contain (oneOf(5, 3, 8))
      javaSet(1, 2, 3) should contain (oneOf(5, 3, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
      
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain (mapMatcher)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
    }
    
    def `should succeeded when right List contains at least one element in right List` {
      List(1, 2, 3) should contain oneOf (5, 3, 8)
      Array(1, 2, 3) should contain oneOf (5, 3, 8)
      javaList(1, 2, 3) should contain oneOf (5, 3, 8)
      
      Set(1, 2, 3) should contain oneOf (5, 3, 8)
      javaSet(1, 2, 3) should contain oneOf (5, 3, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
    }
    
    def `should succeeded when right List contains more than one element in right List` {
      List(1, 2, 3) should contain oneOf (5, 3, 2)
      Array(1, 2, 3) should contain oneOf (5, 3, 2)
      javaList(1, 2, 3) should contain oneOf (5, 3, 2)
      
      Set(1, 2, 3) should contain oneOf (5, 3, 2)
      javaSet(1, 2, 3) should contain oneOf (5, 3, 2)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 2 -> "two")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 2 -> "two")
    }
    
    def `should succeeded when right List contains all elements in left List in different order` {
      List(1, 2, 3) should contain oneOf (1, 3, 2)
      Array(1, 2, 3) should contain oneOf (1, 3, 2)
      javaList(1, 2, 3) should contain oneOf (1, 3, 2)
      
      Set(1, 2, 3) should contain oneOf (1, 3, 2)
      javaSet(1, 2, 3) should contain oneOf (1, 3, 2)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (1 -> "one", 3 -> "three", 2 -> "twp")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (1 -> "one", 3 -> "three", 2 -> "twp")
    }
    
    def `should succeeded when right List contains all elements in left List in same order` {
      List(1, 2, 3) should contain oneOf (1, 2, 3)
      Array(1, 2, 3) should contain oneOf (1, 2, 3)
      javaList(1, 2, 3) should contain oneOf (1, 2, 3)
      
      Set(1, 2, 3) should contain oneOf (1, 2, 3)
      javaSet(1, 2, 3) should contain oneOf (1, 2, 3)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw IllegalArgumentException when oneOf contains duplicate element` {
      val e1 = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e1.getMessage() should be ("oneOf must not contained duplicated value, but 6 is duplicated")
      
      val e2 = intercept[IllegalArgumentException] {
        Set(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e2.getMessage() should be ("oneOf must not contained duplicated value, but 6 is duplicated")
      
      val e3 = intercept[IllegalArgumentException] {
        Array(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e3.getMessage() should be ("oneOf must not contained duplicated value, but 6 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 6, 9)
      val left2 = Set(1, 6, 9)
      val left3 = Map(1 -> "one", 6 -> "six", 9 -> "nine")
      val left4 = javaList(1, 6, 9)
      val left5 = javaSet(1, 6, 9)
      val left6 = javaMap(1 -> "one", 6 -> "six", 9 -> "nine")
      val left7 = Array(1, 6, 9)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e3, left1, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e4, left2, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should contain (mapMatcher)
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
      }
      checkStackDepth(e6, left3, Array(5 -> "five", 3 -> "three", 8 -> "eight"), thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should contain (matcher)
      }
      checkStackDepth(e7, left4, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should contain (matcher)
      }
      checkStackDepth(e8, left5, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e9, left4, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e10, left5, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should contain (mapMatcher)
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")
      }
      checkStackDepth(e12, left6, Array(5 -> "five", 3 -> "three", 8 -> "eight"), thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should contain (matcher)
      }
      checkStackDepth(e13, left7, Array(5, 3, 8).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should contain (oneOf(5, 3, 8))
      }
      checkStackDepth(e14, left7, Array(5, 3, 8).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but does not contain any same element` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e1, left1, Array(7, 8, 9).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e2, left2, Array(7, 8, 9).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e3, left3, Array(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e4, left4, Array(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e5, left5, Array(7, 8, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List and does not contain any same element` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e1, left1, Array(6, 7, 8, 9).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e2, left2, Array(6, 7, 8, 9).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e3, left3, Array(6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e4, left4, Array(6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e5, left5, Array(6, 7, 8, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List and does not contain any same element` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (8, 5)
      }
      checkStackDepth(e1, left1, Array(8, 5).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (8, 5)
      }
      checkStackDepth(e2, left2, Array(8, 5).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (8 -> "eight", 5 -> "five")
      }
      checkStackDepth(e3, left3, Array(8 -> "eight", 5 -> "five"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (8 -> "eight", 5 -> "five")
      }
      checkStackDepth(e4, left4, Array(8 -> "eight", 5 -> "five"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (8, 5)
      }
      checkStackDepth(e5, left5, Array(8, 5).deep, thisLineNumber - 2)
    }
  }
  
  object `not oneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained one of (" + right.mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeed when left List contains different elements as right List` {
      List(1, 2, 3) should not contain oneOf (7, 8, 9)
      Array(1, 2, 3) should not contain oneOf (7, 8, 9)
      javaList(1, 2, 3) should not contain oneOf (7, 8, 9)
      
      Set(1, 2, 3) should not contain oneOf (7, 8, 9)
      javaSet(1, 2, 3) should not contain oneOf (7, 8, 9)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
    }
    
    val matcher = new OneOfContainMatcher(List(5, 7, 9), defaultEquality)
    val mapMatcherRight = Array(5 -> "five", 7 -> "seven", 9 -> "nine")
    val mapMatcher = new OneOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      Array(1, 2, 8) should not contain matcher
      
      javaList(1, 2, 8) should not contain matcher
      javaSet(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain oneOf (5, 7, 9)
      Set(1, 2, 8) should not contain oneOf (5, 7, 9)
      Array(1, 2, 8) should not contain oneOf (5, 7, 9)
      
      javaList(1, 2, 8) should not contain oneOf (5, 7, 9)
      javaSet(1, 2, 8) should not contain oneOf (5, 7, 9)
      
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain oneOf (5 -> "five", 7 -> "seven", 9 -> "nine")
      
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain oneOf (5 -> "five", 7 -> "seven", 9 -> "nine")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 7, 3)
      val left2 = Set(1, 7, 3)
      val left3 = Map(1 -> "one", 7 -> "seven", 3 -> "three")
      val left4 = javaList(1, 7, 3)
      val left5 = javaSet(1, 7, 3)
      val left6 = javaMap(1 -> "one", 7 -> "seven", 3 -> "three")
      val left7 = Array(1, 7, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e3, left1, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e4, left2, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should not contain mapMatcher
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (5 -> "five", 7 -> "seven", 9 -> "nine")
      }
      checkStackDepth(e6, left3, Array(5 -> "five", 7 -> "seven", 9 -> "nine"), thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should not contain matcher
      }
      checkStackDepth(e7, left4, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should not contain matcher
      }
      checkStackDepth(e8, left5, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e9, left4, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e10, left5, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should not contain mapMatcher
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should not contain oneOf (5 -> "five", 7 -> "seven", 9 -> "nine")
      }
      checkStackDepth(e12, left6, Array(5 -> "five", 7 -> "seven", 9 -> "nine"), thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should not contain matcher
      }
      checkStackDepth(e13, left7, Array(5, 7, 9).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should not contain oneOf (5, 7, 9)
      }
      checkStackDepth(e14, left7, Array(5, 7, 9).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain at least one same element` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e1, left1, Array(5, 1, 7).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e2, left2, Array(5, 1, 7).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (5 -> "five", 1 -> "one", 7 -> "seven")
      }
      checkStackDepth(e3, left3, Array(5 -> "five", 1 -> "one", 7 -> "seven"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (5 -> "five", 1 -> "one", 7 -> "seven")
      }
      checkStackDepth(e4, left4, Array(5 -> "five", 1 -> "one", 7 -> "seven"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e5, left5, Array(5, 1, 7).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain more than one same element` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (5, 1, 2)
      }
      checkStackDepth(e1, left1, Array(5, 1, 2).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (5, 1, 2)
      }
      checkStackDepth(e2, left2, Array(5, 1, 2).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (5 -> "five", 1 -> "one", 2 -> "two")
      }
      checkStackDepth(e3, left3, Array(5 -> "five", 1 -> "one", 2 -> "two"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (5 -> "five", 1 -> "one", 2 -> "two")
      }
      checkStackDepth(e4, left4, Array(5 -> "five", 1 -> "one", 2 -> "two"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (5, 1, 2)
      }
      checkStackDepth(e5, left5, Array(5, 1, 2).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain all same element in different order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (3, 2, 1)
      }
      checkStackDepth(e1, left1, Array(3, 2, 1).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (3, 2, 1)
      }
      checkStackDepth(e2, left2, Array(3, 2, 1).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e3, left3, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e4, left4, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (3, 2, 1)
      }
      checkStackDepth(e5, left5, Array(3, 2, 1).deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List contain all same element in same order` {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (1, 2, 3)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (1, 2, 3)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (1, 2, 3)
      }
      checkStackDepth(e5, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
