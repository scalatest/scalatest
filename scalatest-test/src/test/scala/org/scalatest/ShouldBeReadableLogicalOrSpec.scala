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

import SharedHelpers.{thisLineNumber, createTempDirectory}
import Matchers._
import exceptions.TestFailedException

class ShouldBeReadableLogicalOrSpec extends FunSpec {
  
  val fileName: String = "ShouldBeReadableLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(left, right)
  
  def wasNotReadable(left: Any): String = 
    FailureMessages.wasNotReadable(left)
    
  def wasReadable(left: Any): String = 
    FailureMessages.wasReadable(left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }

  trait File { def isReadable: Boolean }

  val readableFile = new File { val isReadable = true }
  
  val secretFile = new File { val isReadable = false }
  
  describe("Sorted matcher") {
    
    describe("when work with 'file should be (readable)'") {
      
      it("should do nothing when file is readable") {
        
        readableFile should (be (readable) or be (readableFile))
        secretFile should (be (readable) or be (secretFile))
        readableFile should (be (readable) or be (secretFile))
        
        readableFile should (be (readableFile) or be (readable))
        readableFile should (be (secretFile) or be (readable))
        secretFile should (be (secretFile) or be (readable))
        
        readableFile should (be (readable) or equal (readableFile))
        secretFile should (be (readable) or equal (secretFile))
        readableFile should (be (readable) or equal (secretFile))
        
        readableFile should (equal (readableFile) or be (readable))
        readableFile should (equal (secretFile) or be (readable))
        secretFile should (equal (secretFile) or be (readable))
      }
      
      it("should throw TestFailedException with correct stack depth when file is not readable") {
        val caught1 = intercept[TestFailedException] {
          secretFile should (be (readable) or be (readableFile))
        }
        assert(caught1.message === Some(wasNotReadable(secretFile) + ", and " + wasNotEqualTo(secretFile, readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          secretFile should (be (readableFile) or be (readable))
        }
        assert(caught2.message === Some(wasNotEqualTo(secretFile, readableFile) + ", and " + wasNotReadable(secretFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          secretFile should (be (readable) or equal (readableFile))
        }
        assert(caught3.message === Some(wasNotReadable(secretFile) + ", and " + didNotEqual(secretFile, readableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          secretFile should (equal (readableFile) or be (readable))
        }
        assert(caught4.message === Some(didNotEqual(secretFile, readableFile) + ", and " + wasNotReadable(secretFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file should not be readable'") {
      
      it("should do nothing when file is not readable") {
        secretFile should (not be readable or not be readableFile)
        readableFile should (not be readable or not be secretFile)
        secretFile should (not be readable or not be secretFile)
        
        secretFile should (not be readableFile or not be readable)
        secretFile should (not be secretFile or not be readable)
        readableFile should (not be secretFile or not be readable)
        
        secretFile should (not be readable or not equal readableFile)
        readableFile should (not be readable or not equal secretFile)
        secretFile should (not be readable or not equal secretFile)
        
        secretFile should (not equal readableFile or not be readable)
        secretFile should (not equal secretFile or not be readable)
        readableFile should (not equal secretFile or not be readable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is readable") {
        val caught1 = intercept[TestFailedException] {
          readableFile should (not be readable or not be readableFile)
        }
        assert(caught1.message === Some(wasReadable(readableFile) + ", and " + wasEqualTo(readableFile, readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          readableFile should (not be readableFile or not be readable)
        }
        assert(caught2.message === Some(wasEqualTo(readableFile, readableFile) + ", and " + wasReadable(readableFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          readableFile should (not be readable or not equal readableFile)
        }
        assert(caught3.message === Some(wasReadable(readableFile) + ", and " + equaled(readableFile, readableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          readableFile should (not equal readableFile or not be readable)
        }
        assert(caught4.message === Some(equaled(readableFile, readableFile) + ", and " + wasReadable(readableFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (readable)'") {
      
      it("should do nothing when all(xs) is readable") {
        all(List(readableFile)) should (be (readable) or be (readableFile))
        all(List(secretFile)) should (be (readable) or be (secretFile))
        all(List(readableFile)) should (be (readable) or be (secretFile))
        
        all(List(readableFile)) should (be (readableFile) or be (readable))
        all(List(readableFile)) should (be (secretFile) or be (readable))
        all(List(secretFile)) should (be (secretFile) or be (readable))
        
        all(List(readableFile)) should (be (readable) or equal (readableFile))
        all(List(secretFile)) should (be (readable) or equal (secretFile))
        all(List(readableFile)) should (be (readable) or equal (secretFile))
        
        all(List(readableFile)) should (equal (readableFile) or be (readable))
        all(List(readableFile)) should (equal (secretFile) or be (readable))
        all(List(secretFile)) should (equal (secretFile) or be (readable))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (readableFile) or be (readable))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(secretFile, readableFile) + ", and " + wasNotReadable(secretFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(secretFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (readable) or be (readableFile))
        }
        assert(caught2.message === Some(allError(wasNotReadable(secretFile) + ", and " + wasNotEqualTo(secretFile, readableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(secretFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (readableFile) or be (readable))
        }
        assert(caught3.message === Some(allError(didNotEqual(secretFile, readableFile) + ", and " + wasNotReadable(secretFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(secretFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (readable) or equal (readableFile))
        }
        assert(caught4.message === Some(allError(wasNotReadable(secretFile) + ", and " + didNotEqual(secretFile, readableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be sorted'") {
      it("should do nothing when xs is not sorted") {
        all(List(secretFile)) should (not be readable or not be readableFile)
        all(List(readableFile)) should (not be readable or not be secretFile)
        all(List(secretFile)) should (not be readable or not be secretFile)
        
        all(List(secretFile)) should (not be readableFile or not be readable)
        all(List(secretFile)) should (not be secretFile or not be readable)
        all(List(readableFile)) should (not be secretFile or not be readable)
        
        all(List(secretFile)) should (not be readable or not equal readableFile)
        all(List(readableFile)) should (not be readable or not equal secretFile)
        all(List(secretFile)) should (not be readable or not equal secretFile)
        
        all(List(secretFile)) should (not equal readableFile or not be readable)
        all(List(secretFile)) should (not equal secretFile or not be readable)
        all(List(readableFile)) should (not equal secretFile or not be readable)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(readableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be readableFile or not be readable)
        }
        assert(caught1.message === Some(allError(wasEqualTo(readableFile, readableFile) + ", and " + wasReadable(readableFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(readableFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be readable or not be readableFile)
        }
        assert(caught2.message === Some(allError(wasReadable(readableFile) + ", and " + wasEqualTo(readableFile, readableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(readableFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal readableFile or not be readable)
        }
        assert(caught3.message === Some(allError(equaled(readableFile, readableFile) + ", and " + wasReadable(readableFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(readableFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be readable or not equal readableFile)
        }
        assert(caught4.message === Some(allError(wasReadable(readableFile) + ", and " + equaled(readableFile, readableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
