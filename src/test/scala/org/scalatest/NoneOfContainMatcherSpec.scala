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

import org.scalatest.words.NoneOfContainMatcher

class NoneOfContainMatcherSpec extends Spec with Matchers with SharedHelpers {

  object `noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained one of (" + right.mkString(", ") + ")"))
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
      javaMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
    }
    
    val matcher = new NoneOfContainMatcher(List(6, 7, 8), defaultEquality)
    val mapMatcherRight = Array(6 -> "six", 7 -> "seven", 8 -> "eight")
    val mapMatcher = new NoneOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 3, 4, 5) should contain (matcher)
      Array(1, 2, 3, 4, 5) should contain (matcher)
      Set(1, 2, 3, 4, 5) should contain (matcher)
      
      javaList(1, 2, 3, 4, 5) should contain (matcher)
      javaSet(1, 2, 3, 4, 5) should contain (matcher)
      
      List(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      Array(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      Set(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      
      javaList(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      javaSet(1, 2, 3, 4, 5) should contain (noneOf(6, 7, 8))
      
      Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain (mapMatcher)
      Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      
      javaMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain (mapMatcher)
      javaMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
    }
    
    def `should succeed when left list contains none of right list` {
      List(1, 2, 3) should contain noneOf (7, 8)
      Array(1, 2, 3) should contain noneOf (7, 8)
      javaList(1, 2, 3) should contain noneOf (7, 8)
      
      Set(1, 2, 3) should contain noneOf (7, 8)
      javaSet(1, 2, 3) should contain noneOf (7, 8)
      
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain noneOf (7 -> "seven", 8 -> "eight")
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain noneOf (7 -> "seven", 8 -> "eight")
    }
    
    def `should throw IllegalArgumentException when noneOf contains duplicate element` {
      val e1 = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e1.getMessage() should be ("noneOf must not contained duplicated value, but 6 is duplicated")
      
      val e2 = intercept[IllegalArgumentException] {
        Set(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e2.getMessage() should be ("noneOf must not contained duplicated value, but 6 is duplicated")
      
      val e3 = intercept[IllegalArgumentException] {
        Array(1, 2, 3) should contain noneOf (6, 8, 6)
      }
      e3.getMessage() should be ("noneOf must not contained duplicated value, but 6 is duplicated")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(1, 2, 7)
      val left2 = Set(1, 2, 7)
      val left3 = Map(1 -> "one", 2 -> "two", 7 -> "seven")
      val left4 = javaList(1, 2, 7)
      val left5 = javaSet(1, 2, 7)
      val left6 = javaMap(1 -> "one", 2 -> "two", 7 -> "seven")
      val left7 = Array(1, 2, 7)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain (matcher)
      }
      checkStackDepth(e1, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain (matcher)
      }
      checkStackDepth(e2, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e3, left1, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e4, left2, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should contain (mapMatcher)
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      }
      checkStackDepth(e6, left3, Array(6 -> "six", 7 -> "seven", 8 -> "eight"), thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should contain (matcher)
      }
      checkStackDepth(e7, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should contain (matcher)
      }
      checkStackDepth(e8, left5, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e9, left4, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e10, left5, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should contain (mapMatcher)
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should contain noneOf (6 -> "six", 7 -> "seven", 8 -> "eight")
      }
      checkStackDepth(e12, left6, Array(6 -> "six", 7 -> "seven", 8 -> "eight"), thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should contain (matcher)
      }
      checkStackDepth(e13, left7, Array(6, 7, 8).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should contain (noneOf(6, 7, 8))
      }
      checkStackDepth(e14, left7, Array(6, 7, 8).deep, thisLineNumber - 2)
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain noneOf (0 -> "zero", 3 -> "three", 8 -> "eight")
      }
      checkStackDepth(e4, left4, Array(0 -> "zero", 3 -> "three", 8 -> "eight"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain noneOf (0, 3, 8)
      }
      checkStackDepth(e5, left5, Array(0, 3, 8).deep, thisLineNumber - 2)
    }
  }
  
  object `not noneOf ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain one of (" + right.mkString(", ") + ")"))
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
      javaMap(1 -> "one", 2 -> "two", 3 -> "three") should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
    }
    
    val matcher = new NoneOfContainMatcher(List(0, 2, 8), defaultEquality)
    val mapMatcherRight = Array(0 -> "zero", 2 -> "two", 8 -> "eight")
    val mapMatcher = new NoneOfContainMatcher(mapMatcherRight, defaultEquality)
    
    def `should work with ContainMatcher directly` {
      
      List(1, 2, 8) should not contain matcher
      Array(1, 2, 8) should not contain matcher
      Set(1, 2, 8) should not contain matcher
      
      javaList(1, 2, 8) should not contain matcher
      javaSet(1, 2, 8) should not contain matcher
      
      List(1, 2, 8) should not contain noneOf (0, 2, 8)
      Array(1, 2, 8) should not contain noneOf (0, 2, 8)
      Set(1, 2, 8) should not contain noneOf (0, 2, 8)
      
      javaList(1, 2, 8) should not contain noneOf (0, 2, 8)
      javaSet(1, 2, 8) should not contain noneOf (0, 2, 8)
      
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      Map(1 -> "one", 2 -> "two", 8 -> "eight") should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
      
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain mapMatcher
      javaMap(1 -> "one", 2 -> "two", 8 -> "eight") should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
    }
    
    def `should throw TestFailedException with correct stack depth and message when used with ContainMatcher directly` {
      val left1 = List(5, 6, 7)
      val left2 = Set(5, 6, 7)
      val left3 = Map(5 -> "five", 6 -> "six", 7 -> "seven")
      val left4 = javaList(5, 6, 7)
      val left5 = javaSet(5, 6, 7)
      val left6 = javaMap(5 -> "five", 6 -> "six", 7 -> "seven")
      val left7 = Array(5, 6, 7)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain matcher
      }
      checkStackDepth(e1, left1, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain matcher
      }
      checkStackDepth(e2, left2, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e3 = intercept[exceptions.TestFailedException] {
        left1 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e3, left1, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e4 = intercept[exceptions.TestFailedException] {
        left2 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e4, left2, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e5 = intercept[exceptions.TestFailedException] {
        left3 should not contain mapMatcher
      }
      checkStackDepth(e5, left3, mapMatcherRight, thisLineNumber - 2)
      
      val e6 = intercept[exceptions.TestFailedException] {
        left3 should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
      }
      checkStackDepth(e6, left3, Array(0 -> "zero", 2 -> "two", 8 -> "eight"), thisLineNumber - 2)
      
      val e7 = intercept[exceptions.TestFailedException] {
        left4 should not contain matcher
      }
      checkStackDepth(e7, left4, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e8 = intercept[exceptions.TestFailedException] {
        left5 should not contain matcher
      }
      checkStackDepth(e8, left5, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e9 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e9, left4, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e10 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e10, left5, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e11 = intercept[exceptions.TestFailedException] {
        left6 should not contain mapMatcher
      }
      checkStackDepth(e11, left6, mapMatcherRight, thisLineNumber - 2)
      
      val e12 = intercept[exceptions.TestFailedException] {
        left6 should not contain noneOf (0 -> "zero", 2 -> "two", 8 -> "eight")
      }
      checkStackDepth(e12, left6, Array(0 -> "zero", 2 -> "two", 8 -> "eight"), thisLineNumber - 2)
      
      val e13 = intercept[exceptions.TestFailedException] {
        left7 should not contain matcher
      }
      checkStackDepth(e13, left7, Array(0, 2, 8).deep, thisLineNumber - 2)
      
      val e14 = intercept[exceptions.TestFailedException] {
        left7 should not contain noneOf (0, 2, 8)
      }
      checkStackDepth(e14, left7, Array(0, 2, 8).deep, thisLineNumber - 2)
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
      
      val left4 = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e4, left4, Array(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noneOf (7, 8, 9)
      }
      checkStackDepth(e5, left5, Array(7, 8, 9).deep, thisLineNumber - 2)
    }
  }
}
