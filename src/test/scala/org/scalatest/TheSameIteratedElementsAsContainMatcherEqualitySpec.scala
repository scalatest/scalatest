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
import SharedHelpers._

class TheSameIteratedElementsAsContainMatcherEqualitySpec extends Spec with Matchers with Explicitly {

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
  
  object `theSameIteratedElementsAs ` {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " did not contain the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.prettifyArrays(left)
      val rightText = FailureMessages.prettifyArrays(right)
      e.message should be (Some(leftText + " contained the same iterated elements as " + rightText))
      e.failedCodeFileName should be (Some("TheSameIteratedElementsAsContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should take custom implicit equality in scope when 'should contain' is used` {
      implicit val equality = new TrimEquality
      List("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")
      Array("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")
      javaList("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")
    }
    
    def `should take custom implicit equality in scope when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      Array(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
      javaList(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3))
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new FalseEquality
      
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameIteratedElementsAs right1
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameIteratedElementsAs right2
      }
        checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameIteratedElementsAs right3
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope` {
      implicit val equality = new TrimEquality
        
      val left1 = List("1 ", " 2", "3 ")
      val right1 = List(" 1", "2 ", " 3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("1 ", " 2", "3 ")
      val right2 = List(" 1", "2 ", " 3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("1 ", " 2", "3 ")
      val right3 = List(" 1", "2 ", " 3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should take passed in custom explicit equality when 'should contain' is used` {
      implicit val equality = new TrimEquality
      (List("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (equality)
      (Array("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (equality)
      (javaList("1 ", " 2", "3 ") should contain theSameIteratedElementsAs List(" 1", "2 ", " 3")) (equality)
    }
    
    def `should take passed in custom explicit equality when 'should not contain' is used` {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
      Array(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
      javaList(1, 2, 3) should not contain theSameIteratedElementsAs (List(1, 2, 3)) (equality)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain theSameIteratedElementsAs right1) (equality)
      }
      checkShouldContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain theSameIteratedElementsAs right2) (equality)
      }
      checkShouldContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList(1, 2, 3)
      val right3 = List(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain theSameIteratedElementsAs right3) (equality)
      }
      checkShouldContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality` {
      implicit val equality = new TrimEquality
        
      val left1 = List("1 ", " 2", "3 ")
      val right1 = List("1", "2 ", " 3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameIteratedElementsAs (right1) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, right1, thisLineNumber - 2)
        
      val left2 = Array("1 ", " 2", "3 ")
      val right2 = List("1", "2 ", " 3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameIteratedElementsAs (right2) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, right2, thisLineNumber - 2)
        
      val left3 = javaList("1 ", " 2", "3 ")
      val right3 = List("1", "2 ", " 3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameIteratedElementsAs (right3) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, right3, thisLineNumber - 2)
    }
  }
  
}
