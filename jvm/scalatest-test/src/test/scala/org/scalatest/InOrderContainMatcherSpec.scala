/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalactic.ColCompatHelper.Iterable
import collection.mutable.LinkedHashMap
import SharedHelpers._
import org.scalactic.Prettifier

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class InOrderContainMatcherSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("inOrder ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " did not contain all of (" + right.map(e => FailureMessages.decorateToStringValue(prettifier, e)).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeeded when left List contains same elements in same order as right List") {
      List(1, 2, 3, 4, 5) should contain inOrder (1, 3, 5)
      Array(1, 2, 3, 4, 5) should contain inOrder (1, 3, 5)
      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3, 4, 5) should contain inOrder (1, 3, 5)
      // SKIP-SCALATESTJS,NATIVE-END
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five").iterator.toStream should contain inOrder (1 -> "one", 3 -> "three", 5 -> "five")
      // javaMap(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain inOrder (1 -> "one", 3 -> "three", 5 -> "five")
    }
    
    it("should succeeded when left List contains same elements in same order as right Set") {
      List(1, 2, 3) should contain inOrder (1, 2, 3)
      Array(1, 2, 3) should contain inOrder (1, 2, 3)
      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain inOrder (1, 2, 3)
      // SKIP-SCALATESTJS,NATIVE-END
      
      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should contain inOrder (1 -> "one", 2 -> "two", 3 -> "three")
      // javaMap(1 -> "one", 2 -> "two", 3 -> "three") should contain inOrder (1 -> "one", 2 -> "two", 3 -> "three")
    }
    
    it("should failed with correct stack depth and message when left List contains same elements in different order as right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e1, left1, deep(Array(2, 1, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e2, left2, deep(Array(2, 1, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrder (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e5, left5, deep(Array(2, 1, 3)), thisLineNumber - 2)
    }
    
    it("should throw NotAllowedException when inOrder contains duplicate element") {
      val e1 = intercept[exceptions.NotAllowedException] {
        List(1, 2, 3) should contain inOrder (1, 2, 1)
      }
      e1.getMessage() should be (FailureMessages.inOrderDuplicate)

      // SKIP-SCALATESTJS,NATIVE-START
      val e2 = intercept[exceptions.NotAllowedException] {
        javaList(1, 2, 3) should contain inOrder (1, 2, 1)
      }
      e2.getMessage() should be (FailureMessages.inOrderDuplicate)
      // SKIP-SCALATESTJS,NATIVE-END

      val e3 = intercept[exceptions.NotAllowedException] {
        Array(1, 2, 3) should contain inOrder (1, 2, 1)
      }
      e3.getMessage() should be (FailureMessages.inOrderDuplicate)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder (2, 5, 3)
      }
      checkStackDepth(e1, left1, deep(Array(2, 5, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder (2, 5, 3)
      }
      checkStackDepth(e2, left2, deep(Array(2, 5, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder (2 -> "two", 5 -> "five", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(2 -> "two", 5 -> "five", 3 -> "three"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrder (2 -> "two", 5 -> "five", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(2 -> "two", 5 -> "five", 3 -> "three"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrder (2, 5, 3)
      }
      checkStackDepth(e5, left5, deep(Array(2, 5, 3)), thisLineNumber - 2)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List contains same elements but in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e1, left1, deep(Array(2, 1, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e2, left2, deep(Array(2, 1, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrder (2 -> "two", 1 -> "one", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrder (2, 1, 3)
      }
      checkStackDepth(e5, left5, deep(Array(2, 1, 3)), thisLineNumber - 2)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List is shorter than right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder (1, 2, 3, 4)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3, 4)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder (1, 2, 3, 4)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3, 4)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrder (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrder (1, 2, 3, 4)
      }
      checkStackDepth(e5, left5, deep(Array(1, 2, 3, 4)), thisLineNumber - 2)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List is longer than right List and right List has different elements") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrder (1, 5)
      }
      checkStackDepth(e1, left1, deep(Array(1, 5)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrder (1, 5)
      }
      checkStackDepth(e2, left2, deep(Array(1, 5)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrder (1 -> "one", 5 -> "five")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 5 -> "five"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrder (1 -> "one", 5 -> "five")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 5 -> "five"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrder (1, 5)
      }
      checkStackDepth(e5, left5, deep(Array(1, 5)), thisLineNumber - 2)
    }
  }
  
  describe("not inOrder ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " contained all of (" + right.map(e => FailureMessages.decorateToStringValue(prettifier, e)).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeeded when left List contains different elements as right List") {
      List(1, 2, 3) should not contain inOrder (1, 2, 8)
      Array(1, 2, 3) should not contain inOrder (1, 2, 8)
      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain inOrder (1, 2, 8)
      // SKIP-SCALATESTJS,NATIVE-END

      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain inOrder (1 -> "one", 2 -> "two", 8 -> "eight")
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain inOrder (1 -> "one", 2 -> "two", 8 -> "eight")
    }
    
    it("should succeeded when left List contains same elements as right List in different order") {
      List(1, 2, 3) should not contain inOrder (1, 3, 2)
      Array(1, 2, 3) should not contain inOrder (1, 3, 2)
      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain inOrder (1, 3, 2)
      // SKIP-SCALATESTJS,NATIVE-END

      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain inOrder (1 -> "one", 3 -> "three", 2 -> "two")
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain inOrder (1 -> "one", 3 -> "three", 2 -> "two")
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List contain same elements in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrder (1, 2, 3)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrder (1, 2, 3)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrder (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain inOrder (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain inOrder (1, 2, 3)
      }
      checkStackDepth(e5, left5, deep(Array(1, 2, 3)), thisLineNumber - 2)
    }
  }
}
