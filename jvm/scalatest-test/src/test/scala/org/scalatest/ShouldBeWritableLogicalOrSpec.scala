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

import SharedHelpers.{thisLineNumber, createTempDirectory}
import java.io.File
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeWritableLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeWritableLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(prettifier, left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(prettifier, left, right)
  
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(prettifier, left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(prettifier, left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }

  // SKIP-SCALATESTJS,NATIVE-START
  val tempDir = createTempDirectory()
  val writableFile = File.createTempFile("writable", "me", tempDir)
  writableFile.setWritable(true)

  val secretFile = new File(tempDir, "imaginary")
  secretFile.setWritable(false)
  // SKIP-SCALATESTJS,NATIVE-END
  //SCALATESTJS,NATIVE-ONLY trait File { def isWritable: Boolean }
  //SCALATESTJS,NATIVE-ONLY val writableFile = new File { val isWritable: Boolean = true }
  //SCALATESTJS,NATIVE-ONLY val secretFile = new File { val isWritable: Boolean = false }
  
  describe("Sorted matcher") {
    
    describe("when work with 'file should be (writable)'") {
      
      it("should do nothing when file is writable") {
        
        writableFile should (be (writable) or be (writableFile))
        secretFile should (be (writable) or be (secretFile))
        writableFile should (be (writable) or be (secretFile))
        
        writableFile should (be (writableFile) or be (writable))
        writableFile should (be (secretFile) or be (writable))
        secretFile should (be (secretFile) or be (writable))
        
        writableFile should (be (writable) or equal (writableFile))
        secretFile should (be (writable) or equal (secretFile))
        writableFile should (be (writable) or equal (secretFile))
        
        writableFile should (equal (writableFile) or be (writable))
        writableFile should (equal (secretFile) or be (writable))
        secretFile should (equal (secretFile) or be (writable))
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile should (be (writable) or be (writableFile))
        }
        assert(caught1.message === Some(wasNotWritable(secretFile) + ", and " + wasNotEqualTo(secretFile, writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          secretFile should (be (writableFile) or be (writable))
        }
        assert(caught2.message === Some(wasNotEqualTo(secretFile, writableFile) + ", and " + wasNotWritable(secretFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          secretFile should (be (writable) or equal (writableFile))
        }
        assert(caught3.message === Some(wasNotWritable(secretFile) + ", and " + didNotEqual(secretFile, writableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          secretFile should (equal (writableFile) or be (writable))
        }
        assert(caught4.message === Some(didNotEqual(secretFile, writableFile) + ", and " + wasNotWritable(secretFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file should not be writable'") {
      
      it("should do nothing when file is not writable") {
        secretFile should (not be writable or not be writableFile)
        writableFile should (not be writable or not be secretFile)
        secretFile should (not be writable or not be secretFile)
        
        secretFile should (not be writableFile or not be writable)
        secretFile should (not be secretFile or not be writable)
        writableFile should (not be secretFile or not be writable)
        
        secretFile should (not be writable or not equal writableFile)
        writableFile should (not be writable or not equal secretFile)
        secretFile should (not be writable or not equal secretFile)
        
        secretFile should (not equal writableFile or not be writable)
        secretFile should (not equal secretFile or not be writable)
        writableFile should (not equal secretFile or not be writable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is writable") {
        val caught1 = intercept[TestFailedException] {
          writableFile should (not be writable or not be writableFile)
        }
        assert(caught1.message === Some(wasWritable(writableFile) + ", and " + wasEqualTo(writableFile, writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          writableFile should (not be writableFile or not be writable)
        }
        assert(caught2.message === Some(wasEqualTo(writableFile, writableFile) + ", and " + wasWritable(writableFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          writableFile should (not be writable or not equal writableFile)
        }
        assert(caught3.message === Some(wasWritable(writableFile) + ", and " + equaled(writableFile, writableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          writableFile should (not equal writableFile or not be writable)
        }
        assert(caught4.message === Some(equaled(writableFile, writableFile) + ", and " + wasWritable(writableFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (writable)'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(writableFile)) should (be (writable) or be (writableFile))
        all(List(secretFile)) should (be (writable) or be (secretFile))
        all(List(writableFile)) should (be (writable) or be (secretFile))
        
        all(List(writableFile)) should (be (writableFile) or be (writable))
        all(List(writableFile)) should (be (secretFile) or be (writable))
        all(List(secretFile)) should (be (secretFile) or be (writable))
        
        all(List(writableFile)) should (be (writable) or equal (writableFile))
        all(List(secretFile)) should (be (writable) or equal (secretFile))
        all(List(writableFile)) should (be (writable) or equal (secretFile))
        
        all(List(writableFile)) should (equal (writableFile) or be (writable))
        all(List(writableFile)) should (equal (secretFile) or be (writable))
        all(List(secretFile)) should (equal (secretFile) or be (writable))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (writableFile) or be (writable))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(secretFile, writableFile) + ", and " + wasNotWritable(secretFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(secretFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (writable) or be (writableFile))
        }
        assert(caught2.message === Some(allError(wasNotWritable(secretFile) + ", and " + wasNotEqualTo(secretFile, writableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(secretFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (writableFile) or be (writable))
        }
        assert(caught3.message === Some(allError(didNotEqual(secretFile, writableFile) + ", and " + wasNotWritable(secretFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(secretFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (writable) or equal (writableFile))
        }
        assert(caught4.message === Some(allError(wasNotWritable(secretFile) + ", and " + didNotEqual(secretFile, writableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be sorted'") {
      it("should do nothing when xs is not sorted") {
        all(List(secretFile)) should (not be writable or not be writableFile)
        all(List(writableFile)) should (not be writable or not be secretFile)
        all(List(secretFile)) should (not be writable or not be secretFile)
        
        all(List(secretFile)) should (not be writableFile or not be writable)
        all(List(secretFile)) should (not be secretFile or not be writable)
        all(List(writableFile)) should (not be secretFile or not be writable)
        
        all(List(secretFile)) should (not be writable or not equal writableFile)
        all(List(writableFile)) should (not be writable or not equal secretFile)
        all(List(secretFile)) should (not be writable or not equal secretFile)
        
        all(List(secretFile)) should (not equal writableFile or not be writable)
        all(List(secretFile)) should (not equal secretFile or not be writable)
        all(List(writableFile)) should (not equal secretFile or not be writable)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be writableFile or not be writable)
        }
        assert(caught1.message === Some(allError(wasEqualTo(writableFile, writableFile) + ", and " + wasWritable(writableFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(writableFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be writable or not be writableFile)
        }
        assert(caught2.message === Some(allError(wasWritable(writableFile) + ", and " + wasEqualTo(writableFile, writableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(writableFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal writableFile or not be writable)
        }
        assert(caught3.message === Some(allError(equaled(writableFile, writableFile) + ", and " + wasWritable(writableFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(writableFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be writable or not equal writableFile)
        }
        assert(caught4.message === Some(allError(wasWritable(writableFile) + ", and " + equaled(writableFile, writableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
