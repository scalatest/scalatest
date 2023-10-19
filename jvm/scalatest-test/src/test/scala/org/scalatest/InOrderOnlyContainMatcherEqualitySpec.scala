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
import org.scalactic.Prettifier
import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InOrderOnlyContainMatcherEqualitySpec extends AnyFunSpec with Matchers with Explicitly {

  private val prettifier = Prettifier.default

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
  
  describe("inOrderOnly ") {
    
    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " did not contain only (" + right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
      
    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " contained only (" + right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ") + ") in order"))
      e.failedCodeFileName should be (Some("InOrderOnlyContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should take custom implicit equality in scope when 'should contain' is used") {
      implicit val equality = new TrimEquality
      List("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
      Array("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
      // SKIP-SCALATESTJS,NATIVE-START
      javaList("1", " 2", "3") should contain inOrderOnly (" 1", "2 ", " 3")
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take custom implicit equality in scope when 'should not contain' is used") {
      implicit val equality = new FalseEquality
      List(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
      Array(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain inOrderOnly (1, 2, 3)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope") {
      implicit val equality = new FalseEquality
      
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrderOnly (1, 2, 3)
      }
      checkShouldContainStackDepth(e3, left3, deep(Array(1, 2, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope") {
      implicit val equality = new TrimEquality
        
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e1, left1, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
        
      val left2 = Array("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e2, left2, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrderOnly (" 1", "2 ", " 3")
      }
      checkShouldNotContainStackDepth(e3, left3, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take passed in custom explicit equality when 'should contain' is used") {
      implicit val equality = new TrimEquality
      (List("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      (Array("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1 ", " 2", "3 ") should contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should take passed in custom explicit equality when 'should not contain' is used") {
      implicit val equality = new FalseEquality
      (List(1, 2, 3) should not contain inOrderOnly (1, 2, 3)) (equality)
      (Array(1, 2, 3) should not contain inOrderOnly (1, 2, 3)) (equality)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList(1, 2, 3) should not contain inOrderOnly (1, 2, 3)) (equality)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality") {
      implicit val equality = new FalseEquality
        
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)
        
      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderOnly (1, 2, 3)) (equality)
      }
      checkShouldContainStackDepth(e3, left3, deep(Array(1, 2, 3)), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality") {
      implicit val equality = new TrimEquality
        
      val left1 = List("1", " 2", "3")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
        
      val left2 = Array("1", " 2", "3")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1", " 2", "3")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderOnly (" 1", "2 ", " 3")) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, deep(Array(" 1", "2 ", " 3")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
