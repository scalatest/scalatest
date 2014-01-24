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

import collection.mutable.LinkedHashMap
import SharedHelpers._
import Matchers._

class TheSameElementsInOrderAsContainMatcherSpec extends Spec {

  object `theSameElementsInOrderAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " did not contain the same elements in the same (iterated) order as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsInOrderAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains same elements in same order as right List` {
      List(1, 2, 3) should contain theSameElementsInOrderAs List(1, 2, 3)
      Array(1, 2, 3) should contain theSameElementsInOrderAs List(1, 2, 3)
      javaList(1, 2, 3) should contain theSameElementsInOrderAs List(1, 2, 3)
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should contain theSameElementsInOrderAs LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three")
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain theSameElementsInOrderAs Map(1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List contains same elements in different order as right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(2, 1, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsInOrderAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val right2 = List(2, 1, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsInOrderAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
      
      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val right3 = LinkedHashMap(2 -> "two", 1 -> "one", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameElementsInOrderAs right3
      }
      checkStackDepth(e3, left3, right3, thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right4 = LinkedHashMap(2 -> "two", 1 -> "one", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain theSameElementsInOrderAs right4
      }
      checkStackDepth(e4, left4, right4, thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val right5 = List(2, 1, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain theSameElementsInOrderAs right5
      }
      checkStackDepth(e5, left5, right5, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 8)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsInOrderAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val right2 = List(1, 2, 8)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsInOrderAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
      
      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val right3 = LinkedHashMap(1 -> "one", 2 -> "two", 8 -> "eight")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameElementsInOrderAs right3
      }
      checkStackDepth(e3, left3, right3, thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right4 = LinkedHashMap(1 -> "one", 2 -> "two", 8 -> "eight")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain theSameElementsInOrderAs right4
      }
      checkStackDepth(e4, left4, right4, thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val right5 = List(1, 2, 8)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain theSameElementsInOrderAs right5
      }
      checkStackDepth(e5, left5, right5, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is shorter than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3, 4)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsInOrderAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val right2 = List(1, 2, 3, 4)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsInOrderAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
      
      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val right3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameElementsInOrderAs right3
      }
      checkStackDepth(e3, left3, right3, thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right4 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain theSameElementsInOrderAs right4
      }
      checkStackDepth(e4, left4, right4, thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val right5 = List(1, 2, 3, 4)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain theSameElementsInOrderAs right5
      }
      checkStackDepth(e5, left5, right5, thisLineNumber - 2)
    }
    
    def `should throw TestFailedException with correct stack depth and message when left List is longer than right List` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain theSameElementsInOrderAs right1
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val right2 = List(1, 2)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain theSameElementsInOrderAs right2
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
      
      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val right3 = LinkedHashMap(1 -> "one", 2 -> "two")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain theSameElementsInOrderAs right3
      }
      checkStackDepth(e3, left3, right3, thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right4 = LinkedHashMap(1 -> "one", 2 -> "two")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain theSameElementsInOrderAs right4
      }
      checkStackDepth(e4, left4, right4, thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val right5 = List(1, 2)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain theSameElementsInOrderAs right5
      }
      checkStackDepth(e5, left5, right5, thisLineNumber - 2)
    }
  }
  
  object `not theSameElementsInOrderAs ` {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Any, lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      val rightText = FailureMessages.decorateToStringValue(right)
      e.message should be (Some(leftText + " contained the same elements in the same (iterated) order as " + rightText))
      e.failedCodeFileName should be (Some("TheSameElementsInOrderAsContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    def `should succeeded when left List contains different elements as right List` {
      List(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 8))
      Array(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 8))
      javaList(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 8))
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two", 8 -> "eight"))
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two", 8 -> "eight"))
    }
    
    def `should succeeded when left List contains less elements than right List` {
      List(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3, 4))
      Array(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3, 4))
      javaList(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2, 3, 4))
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"))
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"))
    }
    
    def `should succeeded when left List contains more elements than right List` {
      List(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2))
      Array(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2))
      javaList(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 2))
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two"))
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 2 -> "two"))
    }
    
    def `should succeeded when left List contains same elements as right List but in different order` {
      List(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 3, 2))
      Array(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 3, 2))
      javaList(1, 2, 3) should not contain theSameElementsInOrderAs (List(1, 3, 2))
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 3 -> "three", 2 -> "two"))
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain theSameElementsInOrderAs (LinkedHashMap(1 -> "one", 3 -> "three", 2 -> "two"))
    }
    
    def `should throw TestFailedException with correct stack depth and message when left and right List are same size but contain same elements in same order` {
      val left1 = List(1, 2, 3)
      val right1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain theSameElementsInOrderAs (right1)
      }
      checkStackDepth(e1, left1, right1, thisLineNumber - 2)
      
      val left2 = javaList(1, 2, 3)
      val right2 = List(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain theSameElementsInOrderAs (right2)
      }
      checkStackDepth(e2, left2, right2, thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val right3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain theSameElementsInOrderAs (right3)
      }
      checkStackDepth(e3, left3, right3, thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val right4 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain theSameElementsInOrderAs (right4)
      }
      checkStackDepth(e4, left4, right4, thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val right5 = List(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain theSameElementsInOrderAs (right5)
      }
      checkStackDepth(e5, left5, right5, thisLineNumber - 2)
    }
  }
}
