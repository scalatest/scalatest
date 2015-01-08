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

import org.scalactic.Equality
import org.scalactic.Explicitly
import org.scalactic.StringNormalizations._
import org.scalactic.Uniformity
import SharedHelpers._
import Matchers._

class TheSameElementsInOrderAsContainMatcherDeciderSpec extends Spec with Explicitly {
  
  val incremented: Uniformity[Int] = 
    new Uniformity[Int] {
      var count = 0
      def normalized(s: Int): Int = {
        count += 1
        s + count
      }
      def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[Int]
      def normalizedOrSame(b: Any): Any =
        b match {
          case i: Int => normalized(i)
          case _ => b
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
  
  object `theSameElementsInOrderAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " did not contain the same elements in the same (iterated) order as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsInOrderAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " contained the same elements in the same (iterated) order as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsInOrderAsContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take specified normalization when 'should contain' is used` {
      (List("1 ", " 2", "3 ") should contain theSameElementsInOrderAs List(" 1", "2 ", " 3")) (after being trimmed)
      (Array("1 ", " 2", "3 ") should contain theSameElementsInOrderAs List(" 1", "2 ", " 3")) (after being trimmed)
      (javaList("1 ", " 2", "3 ") should contain theSameElementsInOrderAs List(" 1", "2 ", " 3")) (after being trimmed)
    }
    
    def `should take specified normalization when 'should not contain' is used` {
      (List(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3))) (after being incremented)
      (Array(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3))) (after being incremented)
      (javaList(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3))) (after being incremented)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization` {
      
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameElementsInOrderAs right1) (after being incremented)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameElementsInOrderAs right2) (after being incremented)
      }
        checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameElementsInOrderAs right3) (after being incremented)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization` {
      
      val left1 = List("1 ", " 2", "3 ")
      val right1 = List(" 1", "2 ", " 3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain theSameElementsInOrderAs (right1)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("1 ", " 2", "3 ")
      val right2 = List(" 1", "2 ", " 3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain theSameElementsInOrderAs (right2)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("1 ", " 2", "3 ")
      val right3 = List(" 1", "2 ", " 3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain theSameElementsInOrderAs (right3)) (after being trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      (List("A ", " B", "C ") should contain theSameElementsInOrderAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", " B", "C ") should contain theSameElementsInOrderAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
      (javaList("A ", " B", "C ") should contain theSameElementsInOrderAs List(" a", "b ", " c")) (decided by lowerCaseEquality afterBeing trimmed)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      (List("one ", " two", "three ") should not contain theSameElementsInOrderAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
      (Array("one ", " two", "three ") should not contain theSameElementsInOrderAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
      (javaList("one ", " two", "three ") should not contain theSameElementsInOrderAs (List(" one", "two ", " three"))) (decided by reverseEquality afterBeing trimmed)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("one ", " two", "three ")
      val right1 = List(" one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameElementsInOrderAs right1) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("one ", " two", "three ")
      val right2 = List(" one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameElementsInOrderAs right2) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("one ", " two", "three ")
      val right3 = List(" one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameElementsInOrderAs right3) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("ONE ", " TWO", "THREE ")
      val right1 = List("one", "two ", " three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain theSameElementsInOrderAs (right1)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("ONE ", " TWO", "THREE ")
      val right2 = List("one", "two ", " three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain theSameElementsInOrderAs (right2)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("ONE ", " TWO", "THREE ")
      val right3 = List("one", "two ", " three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain theSameElementsInOrderAs (right3)) (decided by lowerCaseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
  }
}
