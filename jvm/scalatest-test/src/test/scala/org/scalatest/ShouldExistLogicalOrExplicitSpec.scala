/*
 * Copyright 2001-2025 Artima, Inc.
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

import java.io.File
import SharedHelpers.{createTempDirectory, thisLineNumber}
import enablers.Existence
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldExistLogicalOrExplicitSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  trait Thing {
    def exist: Boolean
  }
  
  val something = new Thing {
    val exist = true
  }
  
  val nothing = new Thing {
    val exist = false
  }
  
  val existence = new Existence[Thing] {
    def exists(thing: Thing): Boolean = thing.exist
  }
  
  val fileName = "ShouldExistLogicalOrExplicitSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages.doesNotExist(prettifier, left)
    
  def exists(left: Any): String = 
    FailureMessages.exists(prettifier, left)
    
  def wasEqualTo(left: Any, right: Any): String =
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotEqualTo(prettifier, left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
  
  def didNotEqual(left: Any, right: Any): String = 
    FailureMessages.didNotEqual(prettifier, left, right)
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
    
  describe("The exist syntax when used with File") {
    
    it("should do nothing when the file exists") {
      (something should (equal (something) or exist)) (defaultEquality, existence)
      (something should (equal (nothing) or exist)) (defaultEquality, existence)
      (nothing should (equal (nothing) or exist)) (defaultEquality, existence)
      
      (something should (exist or equal (something))) (existence, defaultEquality)
      (nothing should (exist or equal (nothing))) (existence, defaultEquality)
      (something should (exist or equal (nothing))) (existence, defaultEquality)
      
      (something should (be (something) or exist)) (existence)
      (something should (be (nothing) or exist)) (existence)
      (nothing should (be (nothing) or exist)) (existence)
      
      (something should (exist or be (something))) (existence)
      (nothing should (exist or be (nothing))) (existence)
      (something should (exist or be (nothing))) (existence)
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val e1 = intercept[exceptions.TestFailedException] {
        (nothing should (equal (something) or exist)) (defaultEquality, existence)
      }
      assert(e1.message === Some(didNotEqual(nothing, something) + ", and " + doesNotExist(nothing)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        (nothing should (exist or equal (something))) (existence, defaultEquality)
      }
      assert(e2.message === Some(doesNotExist(nothing) + ", and " + didNotEqual(nothing, something)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        (nothing should (be (something) or exist)) (existence)
      }
      assert(e3.message === Some(wasNotEqualTo(nothing, something) + ", and " + doesNotExist(nothing)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        (nothing should (exist or be (something))) (existence)
      }
      assert(e4.message === Some(doesNotExist(nothing) + ", and " + wasNotEqualTo(nothing, something)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      (nothing should (equal (nothing) or not (exist))) (defaultEquality, existence)
      (nothing should (equal (something) or not (exist))) (defaultEquality, existence)
      (something should (equal (something) or not (exist))) (defaultEquality, existence)
      
      (nothing should (not (exist) or equal (nothing))) (existence, defaultEquality)
      (something should (not (exist) or equal (something))) (existence, defaultEquality)
      (nothing should (not (exist) or equal (something))) (existence, defaultEquality)
      
      (nothing should (be (nothing) or not (exist))) (existence)
      (nothing should (be (something) or not (exist))) (existence)
      (something should (be (something) or not (exist))) (existence)
      
      (nothing should (not (exist) or be (nothing))) (existence)
      (something should (not (exist) or be (something))) (existence)
      (nothing should (not (exist) or be (something))) (existence)
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val e1 = intercept[exceptions.TestFailedException] {
        (something should (equal (nothing) or not (exist))) (defaultEquality, existence)
      }
      assert(e1.message === Some(didNotEqual(something, nothing) + ", and " + exists(something)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        (something should (not (exist) or equal (nothing))) (existence, defaultEquality)
      }
      assert(e2.message === Some(exists(something) + ", and " + didNotEqual(something, nothing)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        (something should (be (nothing) or not (exist))) (existence)
      }
      assert(e3.message === Some(wasNotEqualTo(something, nothing) + ", and " + exists(something)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        (something should (not (exist) or be (nothing))) (existence)
      }
      assert(e4.message === Some(exists(something) + ", and " + wasNotEqualTo(something, nothing)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  describe("The exist syntax when used with all(xs)") {
    
    it("should do nothing when the file exists") {
      (all(List(something)) should (equal (something) or exist)) (defaultEquality, existence)
      (all(List(something)) should (equal (nothing) or exist)) (defaultEquality, existence)
      (all(List(nothing)) should (equal (nothing) or exist)) (defaultEquality, existence)
      
      (all(List(something)) should (exist or equal (something))) (existence, defaultEquality)
      (all(List(nothing)) should (exist or equal (nothing))) (existence, defaultEquality)
      (all(List(something)) should (exist or equal (nothing))) (existence, defaultEquality)
      
      (all(List(something)) should (be (something) or exist)) (existence)
      (all(List(something)) should (be (nothing) or exist)) (existence)
      (all(List(nothing)) should (be (nothing) or exist)) (existence)
      
      (all(List(something)) should (exist or be (something))) (existence)
      (all(List(nothing)) should (exist or be (nothing))) (existence)
      (all(List(something)) should (exist or be (nothing))) (existence)
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val left1 = List(nothing)
      val e1 = intercept[exceptions.TestFailedException] {
        (all(left1) should (equal (something) or exist)) (defaultEquality, existence)
      }
      assert(e1.message === Some(allError(left1, didNotEqual(nothing, something) + ", and " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(nothing)
      val e2 = intercept[exceptions.TestFailedException] {
        (all(left2) should (exist or equal (something))) (existence, defaultEquality)
      }
      assert(e2.message === Some(allError(left2, doesNotExist(nothing) + ", and " + didNotEqual(nothing, something), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(nothing)
      val e3 = intercept[exceptions.TestFailedException] {
        (all(left3) should (be (something) or exist)) (existence)
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(nothing, something) + ", and " + doesNotExist(nothing), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(nothing)
      val e4 = intercept[exceptions.TestFailedException] {
        (all(left4) should (exist or be (something))) (existence)
      }
      assert(e4.message === Some(allError(left4, doesNotExist(nothing) + ", and " + wasNotEqualTo(nothing, something), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      (all(List(nothing)) should (equal (nothing) or not (exist))) (defaultEquality, existence)
      (all(List(nothing)) should (equal (something) or not (exist))) (defaultEquality, existence)
      (all(List(something)) should (equal (something) or not (exist))) (defaultEquality, existence)
      
      (all(List(nothing)) should (not (exist) or equal (nothing))) (existence, defaultEquality)
      (all(List(something)) should (not (exist) or equal (something))) (existence, defaultEquality)
      (all(List(nothing)) should (not (exist) or equal (something))) (existence, defaultEquality)
      
      (all(List(nothing)) should (be (nothing) or not (exist))) (existence)
      (all(List(nothing)) should (be (something) or not (exist))) (existence)
      (all(List(something)) should (be (something) or not (exist))) (existence)
      
      (all(List(nothing)) should (not (exist) or be (nothing))) (existence)
      (all(List(something)) should (not (exist) or be (something))) (existence)
      (all(List(nothing)) should (not (exist) or be (something))) (existence)
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val left1 = List(something)
      val e1 = intercept[exceptions.TestFailedException] {
        (all(left1) should (equal (nothing) or not (exist))) (defaultEquality, existence)
      }
      assert(e1.message === Some(allError(left1, didNotEqual(something, nothing) + ", and " + exists(something), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(something)
      val e2 = intercept[exceptions.TestFailedException] {
        (all(left2) should (not (exist) or equal (nothing))) (existence, defaultEquality)
      }
      assert(e2.message === Some(allError(left2, exists(something) + ", and " + didNotEqual(something, nothing), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(something)
      val e3 = intercept[exceptions.TestFailedException] {
        (all(left3) should (be (nothing) or not (exist))) (existence)
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(something, nothing) + ", and " + exists(something), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(something)
      val e4 = intercept[exceptions.TestFailedException] {
        (all(left4) should (not (exist) or be (nothing))) (existence)
      }
      assert(e4.message === Some(allError(left4, exists(something) + ", and " + wasNotEqualTo(something, nothing), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
  }
}