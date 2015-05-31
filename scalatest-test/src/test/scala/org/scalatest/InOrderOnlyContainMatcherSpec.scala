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
import collection.mutable.LinkedHashMap
import SharedHelpers._
import Matchers._
import org.scalatest.exceptions.TestFailedException

class InOrderOnlyContainMatcherSpec extends FunSpec {

  describe("inOrderOnly ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(leftText + " did not contain only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeed when left List contains elements available in right List") {
      List(4, 4, 4, 5, 5, 6, 6) should contain inOrderOnly (4, 5, 6)
      Array(4, 4, 4, 5, 5, 6, 6) should contain inOrderOnly (4, 5, 6)
      // SKIP-SCALATESTJS-START
      javaList(4, 4, 4, 5, 5, 6, 6) should contain inOrderOnly (4, 5, 6)
      // SKIP-SCALATESTJS-END
      
      LinkedHashMap(4 -> "four", 5 -> "five", 6 -> "six").iterator.toStream should contain inOrderOnly (4 -> "four", 5 -> "five", 6 -> "six")
      // javaMap(Entry(4, "four"), Entry(5, "five"), Entry(6, "six")) should contain inOrderOnly (4 -> "four", 5 -> "five", 6 -> "six")
    }
    
    it("should fail when left list contains part of right list") {
      intercept[TestFailedException] {
        List(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 2, 3, 4, 5)
      }
      intercept[TestFailedException] {
        Array(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 2, 3, 4, 5)
      }
      // SKIP-SCALATESTJS-START
      intercept[TestFailedException] {
        javaList(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 2, 3, 4, 5)
      }
      // SKIP-SCALATESTJS-END
      
      intercept[TestFailedException] {
        LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should contain inOrderOnly (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
      }
/*
      intercept[TestFailedException] {
        javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain inOrderOnly (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
      }
*/
    }

    // TODO: Chee Seng, let's make this do a NotAllowedException with good stack depth.
    ignore("should throw IllegalArgumentException when inOrderOnly contains duplicate element") {
      val e1 = intercept[IllegalArgumentException] {
        List(1, 2, 3) should contain inOrderOnly (1, 2, 1)
      }
      e1.getMessage() should be ("inOrderOnly must not contained duplicated value, but 1 is duplicated")
      
      val e2 = intercept[IllegalArgumentException] {
        Array(1, 2, 3) should contain inOrderOnly (1, 2, 1)
      }
      e2.getMessage() should be ("inOrderOnly must not contained duplicated value, but 1 is duplicated")
    }

    it("should throw TestFailedException with correct stack depth and message when left List contains element not in right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrderOnly (1, 2)
      }
      checkStackDepth(e1, left1, Array(1, 2).deep, thisLineNumber - 2)

      // SKIP-SCALATESTJS-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrderOnly (1, 2)
      }
      checkStackDepth(e2, left2, Array(1, 2).deep, thisLineNumber - 2)
      // SKIP-SCALATESTJS-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrderOnly (1 -> "one", 2 -> "two")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrderOnly (1 -> "one", 2 -> "two")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrderOnly (1, 2)
      }
      checkStackDepth(e5, left5, Array(1, 2).deep, thisLineNumber - 2)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List contains only element in right List, but in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrderOnly (3, 2, 1)
      }
      checkStackDepth(e1, left1, Array(3, 2, 1).deep, thisLineNumber - 2)

      // SKIP-SCALATESTJS-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrderOnly (3, 2, 1)
      }
      checkStackDepth(e2, left2, Array(3, 2, 1).deep, thisLineNumber - 2)
      // SKIP-SCALATESTJS-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrderOnly (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e3, left3, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain inOrderOnly (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e4, left4, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain inOrderOnly (3, 2, 1)
      }
      checkStackDepth(e5, left5, Array(3, 2, 1).deep, thisLineNumber - 2)
    }
  }
  
  describe("not inOrderOnly ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(leftText + " contained only (" + right.mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeed when left List contains element not in right List") {
      List(1, 2, 3) should not contain inOrderOnly (1, 2)
      Array(1, 2, 3) should not contain inOrderOnly (1, 2)
      // SKIP-SCALATESTJS-START
      javaList(1, 2, 3) should not contain inOrderOnly (1, 2)
      // SKIP-SCALATESTJS-END

      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain inOrderOnly (1 -> "one", 2 -> "two")
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain inOrderOnly (1 -> "one", 2 -> "two")
    }
    
    it("should succeed when left List contains element in right List but in different order") {
      List(1, 2, 3) should not contain inOrderOnly (3, 2, 1)
      Array(1, 2, 3) should not contain inOrderOnly (3, 2, 1)
      // SKIP-SCALATESTJS-START
      javaList(1, 2, 3) should not contain inOrderOnly (3, 2, 1)
      // SKIP-SCALATESTJS-END

      LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream should not contain inOrderOnly (3 -> "three", 2 -> "two", 1 -> "one")
      // javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain inOrderOnly (3 -> "three", 2 -> "two", 1 -> "one")
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e1, left1, Array(1, 2, 3).deep, thisLineNumber - 2)

      // SKIP-SCALATESTJS-START
      val left2 = javaList(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e2, left2, Array(1, 2, 3).deep, thisLineNumber - 2)
      // SKIP-SCALATESTJS-END

      val left3 = LinkedHashMap(1 -> "one", 2 -> "two", 3 -> "three").iterator.toStream
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrderOnly (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
      
/*
      val left4 = javaMap(Entry(1, "one"), Entry(2, "two)", Entry(3, "three"))
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain inOrderOnly (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e4, left4, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)
*/
      
      val left5 = Array(1, 2, 3)
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain inOrderOnly (1, 2, 3)
      }
      checkStackDepth(e5, left5, Array(1, 2, 3).deep, thisLineNumber - 2)
    }
  }
}
