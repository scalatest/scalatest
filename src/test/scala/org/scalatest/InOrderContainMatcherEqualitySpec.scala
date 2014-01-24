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
import Matchers._

class InOrderContainMatcherEqualitySpec extends Spec with Explicitly {
  
  class CustomEquality extends Equality[String] {
    def areEqual(left: String, right: Any) = 
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }
  
  object `inOrder ` {
    
    implicit val equality = new CustomEquality
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(leftText + " did not contain all of (" + right.map(FailureMessages.decorateToStringValue).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(leftText + " contained all of (" + right.map(FailureMessages.decorateToStringValue).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      List("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")
      Array("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")
      javaList("1", "2 ", "3") should contain inOrder ("1", "2 ", "3")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      List("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")
      Array("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")
      javaList("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder ("3", "2 ", "1")
      }
      checkShouldContainStackDepth(e1, left1, Array("3", "2 ", "1").deep, thisLineNumber - 2)
        
      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder ("3", "2 ", "1")
      }
        checkShouldContainStackDepth(e2, left2, Array("3", "2 ", "1").deep, thisLineNumber - 2)
        
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder ("3", "2 ", "1")
      }
      checkShouldContainStackDepth(e3, left3, Array("3", "2 ", "1").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrder ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e1, left1, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrder ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e2, left2, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrder ("1", "2 ", "3")
      }
      checkShouldNotContainStackDepth(e3, left3, Array("1", "2 ", "3").deep, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      (List("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")) (equality)
      (Array("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")) (equality)
      (javaList("1 ", "2", "3 ") should contain inOrder ("1", "2 ", "3")) (equality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      (List("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")) (equality)
      (Array("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")) (equality)
      (javaList("1 ", "2", "3 ") should not contain inOrder ("3", "2 ", "1")) (equality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrder ("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Array("3", "2 ", "1").deep, thisLineNumber - 2)
        
      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrder ("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Array("3", "2 ", "1").deep, thisLineNumber - 2)
        
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrder ("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e3, left3, Array("3", "2 ", "1").deep, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      
      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrder ("1", "2 ", "3")) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrder ("1", "2 ", "3")) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Array("1", "2 ", "3").deep, thisLineNumber - 2)
        
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrder ("1", "2 ", "3")) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Array("1", "2 ", "3").deep, thisLineNumber - 2)
    }
  }
}
