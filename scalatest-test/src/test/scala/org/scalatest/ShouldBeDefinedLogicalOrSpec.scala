/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this opt except in compliance with the License.
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

import SharedHelpers.thisLineNumber
import Matchers._
import exceptions.TestFailedException

class ShouldBeDefinedLogicalOrSpec extends FunSpec {
  
  val fileName: String = "ShouldBeDefinedLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(left, right)
  
  def wasNotDefined(left: Any): String = 
    FailureMessages.wasNotDefined(left)
    
  def wasDefined(left: Any): String = 
    FailureMessages.wasDefined(left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
  
  val something = Some("Something")
  val nothing = None
  
  describe("Defined matcher") {
    
    describe("when work with 'opt should be (defined)'") {
      
      it("should do nothing when opt is defined") {
        
        something should (be (defined) or be (something))
        nothing should (be (defined) or be (nothing))
        something should (be (defined) or be (nothing))
        
        something should (be (something) or be (defined))
        something should (be (nothing) or be (defined))
        nothing should (be (nothing) or be (defined))
        
        something should (be (defined) or equal (something))
        nothing should (be (defined) or equal (nothing))
        something should (be (defined) or equal (nothing))
        
        something should (equal (something) or be (defined))
        something should (equal (nothing) or be (defined))
        nothing should (equal (nothing) or be (defined))
      }
      
      it("should throw TestFailedException with correct stack depth when opt is not defined") {
        val caught1 = intercept[TestFailedException] {
          nothing should (be (defined) or be (something))
        }
        assert(caught1.message === Some(wasNotDefined(nothing) + ", and " + wasNotEqualTo(nothing, something)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          nothing should (be (something) or be (defined))
        }
        assert(caught2.message === Some(wasNotEqualTo(nothing, something) + ", and " + wasNotDefined(nothing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          nothing should (be (defined) or equal (something))
        }
        assert(caught3.message === Some(wasNotDefined(nothing) + ", and " + didNotEqual(nothing, something)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          nothing should (equal (something) or be (defined))
        }
        assert(caught4.message === Some(didNotEqual(nothing, something) + ", and " + wasNotDefined(nothing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'opt should not be defined'") {
      
      it("should do nothing when opt is not defined") {
        nothing should (not be defined or not be something)
        something should (not be defined or not be nothing)
        nothing should (not be defined or not be nothing)
        
        nothing should (not be something or not be defined)
        nothing should (not be nothing or not be defined)
        something should (not be nothing or not be defined)
        
        nothing should (not be defined or not equal something)
        something should (not be defined or not equal nothing)
        nothing should (not be defined or not equal nothing)
        
        nothing should (not equal something or not be defined)
        nothing should (not equal nothing or not be defined)
        something should (not equal nothing or not be defined)
      }
      
      it("should throw TestFailedException with correct stack depth when opt is defined") {
        val caught1 = intercept[TestFailedException] {
          something should (not be defined or not be something)
        }
        assert(caught1.message === Some(wasDefined(something) + ", and " + wasEqualTo(something, something)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          something should (not be something or not be defined)
        }
        assert(caught2.message === Some(wasEqualTo(something, something) + ", and " + wasDefined(something)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          something should (not be defined or not equal something)
        }
        assert(caught3.message === Some(wasDefined(something) + ", and " + equaled(something, something)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          something should (not equal something or not be defined)
        }
        assert(caught4.message === Some(equaled(something, something) + ", and " + wasDefined(something)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (defined)'") {
      
      it("should do nothing when all(xs) is defined") {
        all(List(something)) should (be (defined) or be (something))
        all(List(nothing)) should (be (defined) or be (nothing))
        all(List(something)) should (be (defined) or be (nothing))
        
        all(List(something)) should (be (something) or be (defined))
        all(List(something)) should (be (nothing) or be (defined))
        all(List(nothing)) should (be (nothing) or be (defined))
        
        all(List(something)) should (be (defined) or equal (something))
        all(List(nothing)) should (be (defined) or equal (nothing))
        all(List(something)) should (be (defined) or equal (nothing))
        
        all(List(something)) should (equal (something) or be (defined))
        all(List(something)) should (equal (nothing) or be (defined))
        all(List(nothing)) should (equal (nothing) or be (defined))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(nothing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (something) or be (defined))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(nothing, something) + ", and " + wasNotDefined(nothing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nothing)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (defined) or be (something))
        }
        assert(caught2.message === Some(allError(wasNotDefined(nothing) + ", and " + wasNotEqualTo(nothing, something), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nothing)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (something) or be (defined))
        }
        assert(caught3.message === Some(allError(didNotEqual(nothing, something) + ", and " + wasNotDefined(nothing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nothing)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (defined) or equal (something))
        }
        assert(caught4.message === Some(allError(wasNotDefined(nothing) + ", and " + didNotEqual(nothing, something), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be sorted'") {
      it("should do nothing when xs is not sorted") {
        all(List(nothing)) should (not be defined or not be something)
        all(List(something)) should (not be defined or not be nothing)
        all(List(nothing)) should (not be defined or not be nothing)
        
        all(List(nothing)) should (not be something or not be defined)
        all(List(nothing)) should (not be nothing or not be defined)
        all(List(something)) should (not be nothing or not be defined)
        
        all(List(nothing)) should (not be defined or not equal something)
        all(List(something)) should (not be defined or not equal nothing)
        all(List(nothing)) should (not be defined or not equal nothing)
        
        all(List(nothing)) should (not equal something or not be defined)
        all(List(nothing)) should (not equal nothing or not be defined)
        all(List(something)) should (not equal nothing or not be defined)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(something)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be something or not be defined)
        }
        assert(caught1.message === Some(allError(wasEqualTo(something, something) + ", and " + wasDefined(something), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(something)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be defined or not be something)
        }
        assert(caught2.message === Some(allError(wasDefined(something) + ", and " + wasEqualTo(something, something), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(something)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal something or not be defined)
        }
        assert(caught3.message === Some(allError(equaled(something, something) + ", and " + wasDefined(something), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(something)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be defined or not equal something)
        }
        assert(caught4.message === Some(allError(wasDefined(something) + ", and " + equaled(something, something), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
