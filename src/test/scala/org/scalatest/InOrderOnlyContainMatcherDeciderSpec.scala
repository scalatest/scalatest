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
import org.scalautils.StringNormalizations
import org.scalautils.Normalization
import collection.GenTraversable
import SharedHelpers._

class InOrderOnlyContainMatcherDeciderSpec extends Spec with Matchers with Explicitly with StringNormalizations {
  
  val incremented: Normalization[Int] = 
    new Normalization[Int] {
      var count = 0
      def normalizedIfInstanceOfA(b: Any) =
        b match {
          case i: Int => normalized(i)
          case _ => b
        }
    
      def normalized(s: Int): Int = {
        count += 1
        s + count
      }
    }
 
  val appended: Normalization[String] = 
    new Normalization[String] {
      var count = 0
      def normalizedIfInstanceOfA(b: Any) =
        b match {
          case s: String => normalized(s)
          case _ => b
        }
    
      def normalized(s: String): String = {
        count += 1
        s + count
      }
    }
  
  val lowerCaseEquality = 
    new Equality[String] {
      def areEqual(left: String, right: Any) = 
        left.toLowerCase == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }
  
  val reverseEquality = 
    new Equality[String] {
      def areEqual(left: String, right: Any) = 
        left.reverse == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }
  
  object `inOrderOnly ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " did not contain only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      e.message should be (Some(leftText + " contained only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take specified normalization when 'should contain' is used` {
      (List("1", " 2", "3") should contain inOrderOnly) (" 1", "2 ", " 3") (after being trimmed)
      (Array("1", " 2", "3") should contain inOrderOnly) (" 1", "2 ", " 3") (after being trimmed)
      (javaList("1", " 2", "3") should contain inOrderOnly) (" 1", "2 ", " 3") (after being trimmed)
    }
    
    def `should take specified normalization when 'should not contain' is used` {
      (List("1", "2", "3") should not contain inOrderOnly ("1", "2", "3")) (after being appended)
      (Array("1", "2", "3") should not contain inOrderOnly ("1", "2", "3")) (after being appended)
      (javaList("1", "2", "3") should not contain inOrderOnly ("1", "2", "3")) (after being appended)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization` {
      
      val left1 = List("1", "2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderOnly ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e1, left1, Array("1", "2", "3").deep, thisLineNumber - 2)
        
      val left2 = Array("1", "2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderOnly ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e2, left2, Array("1", "2", "3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1", "2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderOnly ("1", "2", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e3, left3, Array("1", "2", "3").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization` {
        
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderOnly (" 1", "2 ", " 3")) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left2 = Array("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderOnly (" 1", "2 ", " 3")) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderOnly (" 1", "2 ", " 3")) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(" 1", "2 ", " 3").deep, thisLineNumber - 2)
    }
    
    def `should take specified equality and normalization when 'should contain' is used` {
      (List("ONE ", " TWO", "THREE ") should contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("ONE ", " TWO", "THREE ") should contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("ONE ", " TWO", "THREE ") should contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
    }
    
    def `should take specified equality and normalization when 'should not contain' is used` {
      (List("one ", " two", "three ") should not contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      (Array("one ", " two", "three ") should not contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      (javaList("one ", " two", "three ") should not contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("one ", " two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, Array(" one", "two ", " three").deep, thisLineNumber - 2)
        
      val left2 = Array("one ", " two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, Array(" one", "two ", " three").deep, thisLineNumber - 2)
        
      val left3 = javaList("one ", " two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderOnly (" one", "two ", " three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, Array(" one", "two ", " three").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization` {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, Array(" one", "two ", " three").deep, thisLineNumber - 2)
        
      val left2 = Array("ONE ", " TWO", "THREE ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, Array(" one", "two ", " three").deep, thisLineNumber - 2)
        
      val left3 = javaList("ONE ", " TWO", "THREE ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderOnly (" one", "two ", " three")) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, Array(" one", "two ", " three").deep, thisLineNumber - 2)
    }
    
  }
  
}
