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

import SharedHelpers.thisLineNumber
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeDefinedSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val something = Some("Something")
  val nothing = None
  
  val fileName: String = "ShouldBeDefinedSpec.scala"
    
  def wasNotDefined(left: Any): String = 
    FailureMessages.wasNotDefined(prettifier, left)
    
  def wasDefined(left: Any): String = 
    FailureMessages.wasDefined(prettifier, left)
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
  
  describe("defined matcher") {
    
    describe("when work with 'opt should be (defined)'") {
      
      it("should do nothing when opt is defined") {
        something should be (defined)
      }
      
      it("should throw TestFailedException with correct stack depth when opt is not defined") {
        val caught1 = intercept[TestFailedException] {
          nothing should be (defined)
        }
        assert(caught1.message === Some(wasNotDefined(nothing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'opt should not be defined'") {
      
      it("should do nothing when opt is not defined") {
        nothing should not be defined
      }
      
      it("should throw TestFailedException with correct stack depth when opt is defined") {
        val caught1 = intercept[TestFailedException] {
          something should not be defined
        }
        assert(caught1.message === Some(wasDefined(something)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'opt shouldBe defined'") {
      
      it("should do nothing when opt is defined") {
        something shouldBe defined
      }
      
      it("should throw TestFailedException with correct stack depth when opt is not defined") {
        val caught1 = intercept[TestFailedException] {
          nothing shouldBe defined
        }
        assert(caught1.message === Some(wasNotDefined(nothing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'opt shouldNot be (defined)'") {
      
      it("should do nothing when opt is not defined") {
        nothing shouldNot be (defined)
      }
      
      it("should throw TestFailedException with correct stack depth when opt is defined") {
        val caught1 = intercept[TestFailedException] {
          something shouldNot be (defined)
        }
        assert(caught1.message === Some(wasDefined(something)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should be (defined)'") {
      
      it("should do nothing when all(xs) is defined") {
        all(List(something)) should be (defined)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not defined") {
        val left1 = List(nothing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (defined)
        }
        assert(caught1.message === Some(allError(left1, wasNotDefined(nothing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should not be defined'") {
      
      it("should do nothing when all(xs) is not defined") {
        all(List(nothing)) should not be defined
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is defined") {
        val left1 = List(something)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be defined
        }
        assert(caught1.message === Some(allError(left1, wasDefined(something), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) shouldBe defined'") {
      
      it("should do nothing when all(xs) is defined") {
        all(List(something)) shouldBe defined
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not defined") {
        val left1 = List(nothing)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe defined
        }
        assert(caught1.message === Some(allError(left1, wasNotDefined(nothing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) shouldNot be (defined)'") {
      
      it("should do nothing when all(xs) is not defined") {
        all(List(nothing)) shouldNot be (defined)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is defined") {
        val left1 = List(something)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (defined)
        }
        assert(caught1.message === Some(allError(left1, wasDefined(something), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
