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
import java.io.File
import Matchers._
import exceptions.TestFailedException
import org.scalactic.Prettifier

class ShouldBeWritableLogicalAndSpec extends FunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeWritableLogicalAndSpec.scala"
  
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
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }

  // SKIP-SCALATESTJS-START
  val tempDir = createTempDirectory()
  val writableFile = File.createTempFile("writable", "me", tempDir)
  writableFile.setWritable(true)
  
  val secretFile = new File(tempDir, "imaginary")
  secretFile.setWritable(false)
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY trait File { def isWritable: Boolean }
  //SCALATESTJS-ONLY val writableFile = new File { val isWritable: Boolean = true }
  //SCALATESTJS-ONLY val secretFile = new File { val isWritable: Boolean = false }
  
  describe("Readability matcher") {
    
    describe("when work with 'file should be (writable)'") {
      
      it("should do nothing when file is writable") {
        writableFile should (equal (writableFile) and be (writable))
        writableFile should (be (writable) and equal (writableFile))
        
        writableFile should (be (writableFile) and be (writable))
        writableFile should (be (writable) and be (writableFile))
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile should (equal (secretFile) and be (writable))
        }
        assert(caught1.message === Some(equaled(secretFile, secretFile) + ", but " + wasNotWritable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          secretFile should (be (writable) and equal (secretFile))
        }
        assert(caught2.message === Some(wasNotWritable(secretFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          secretFile should (be (secretFile) and be (writable))
        }
        assert(caught3.message === Some(wasEqualTo(secretFile, secretFile) + ", but " + wasNotWritable(secretFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          secretFile should (be (writable) and be (secretFile))
        }
        assert(caught4.message === Some(wasNotWritable(secretFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file should not be sorted'") {
      
      it("should do nothing when file is not writable") {
        secretFile should (not equal writableFile and not be writable)
        secretFile should (not be writable and not equal writableFile)
        
        secretFile should (not be writableFile and not be writable)
        secretFile should (not be writable and not be writableFile)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val caught1 = intercept[TestFailedException] {
          writableFile should (not equal secretFile and not be writable)
        }
        assert(caught1.message === Some(didNotEqual(writableFile, secretFile) + ", but " + wasWritable(writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          writableFile should (not be writable and not equal secretFile)
        }
        assert(caught2.message === Some(wasWritable(writableFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          writableFile should (not be secretFile and not be writable)
        }
        assert(caught3.message === Some(wasNotEqualTo(writableFile, secretFile) + ", but " + wasWritable(writableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          writableFile should (not be writable and not be secretFile)
        }
        assert(caught4.message === Some(wasWritable(writableFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'path should be (writable)'") {

      it("should do nothing when path is writable") {
        writableFile.toPath should (equal (writableFile.toPath) and be (writable))
        writableFile.toPath should (be (writable) and equal (writableFile.toPath))

        writableFile.toPath should (be (writableFile.toPath) and be (writable))
        writableFile.toPath should (be (writable) and be (writableFile.toPath))
      }

      it("should throw TestFailedException with correct stack depth when path is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile.toPath should (equal (secretFile.toPath) and be (writable))
        }
        assert(caught1.message === Some(equaled(secretFile.toPath, secretFile.toPath) + ", but " + wasNotWritable(secretFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          secretFile.toPath should (be (writable) and equal (secretFile.toPath))
        }
        assert(caught2.message === Some(wasNotWritable(secretFile.toPath)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          secretFile.toPath should (be (secretFile.toPath) and be (writable))
        }
        assert(caught3.message === Some(wasEqualTo(secretFile.toPath, secretFile.toPath) + ", but " + wasNotWritable(secretFile.toPath)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught4 = intercept[TestFailedException] {
          secretFile.toPath should (be (writable) and be (secretFile.toPath))
        }
        assert(caught4.message === Some(wasNotWritable(secretFile.toPath)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'path should not be sorted'") {

      it("should do nothing when path is not writable") {
        secretFile.toPath should (not equal writableFile.toPath and not be writable)
        secretFile.toPath should (not be writable and not equal writableFile.toPath)

        secretFile.toPath should (not be writableFile.toPath and not be writable)
        secretFile.toPath should (not be writable and not be writableFile.toPath)
      }

      it("should throw TestFailedException with correct stack depth when paths is not sorted") {
        val caught1 = intercept[TestFailedException] {
          writableFile.toPath should (not equal secretFile.toPath and not be writable)
        }
        assert(caught1.message === Some(didNotEqual(writableFile.toPath, secretFile.toPath) + ", but " + wasWritable(writableFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          writableFile.toPath should (not be writable and not equal secretFile.toPath)
        }
        assert(caught2.message === Some(wasWritable(writableFile.toPath)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          writableFile.toPath should (not be secretFile.toPath and not be writable)
        }
        assert(caught3.message === Some(wasNotEqualTo(writableFile.toPath, secretFile.toPath) + ", but " + wasWritable(writableFile.toPath)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught4 = intercept[TestFailedException] {
          writableFile.toPath should (not be writable and not be secretFile.toPath)
        }
        assert(caught4.message === Some(wasWritable(writableFile.toPath)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (writable)'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(writableFile)) should (be (writableFile) and be (writable))
        all(List(writableFile)) should (be (writable) and be (writableFile))
        
        all(List(writableFile)) should (equal (writableFile) and be (writable))
        all(List(writableFile)) should (be (writable) and equal (writableFile))
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not writable") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (secretFile) and be (writable))
        }
        assert(caught1.message === Some(allError(wasEqualTo(secretFile, secretFile) + ", but " + wasNotWritable(secretFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(secretFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (writable) and be (secretFile))
        }
        assert(caught2.message === Some(allError(wasNotWritable(secretFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(secretFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (secretFile) and be (writable))
        }
        assert(caught3.message === Some(allError(equaled(secretFile, secretFile) + ", but " + wasNotWritable(secretFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(secretFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (writable) and equal (secretFile))
        }
        assert(caught4.message === Some(allError(wasNotWritable(secretFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be writable'") {
      it("should do nothing when all(xs) is not writable") {
        all(List(secretFile)) should (not be writable and not be writableFile)
        all(List(secretFile)) should (not be writableFile and not be writable)
        
        all(List(secretFile)) should (not be writable and not equal writableFile)
        all(List(secretFile)) should (not equal writableFile and not be writable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is writable") {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be secretFile and not be writable)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(writableFile, secretFile) + ", but " + wasWritable(writableFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(writableFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be writable and not be secretFile)
        }
        assert(caught2.message === Some(allError(wasWritable(writableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(writableFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal secretFile and not be writable)
        }
        assert(caught3.message === Some(allError(didNotEqual(writableFile, secretFile) + ", but " + wasWritable(writableFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(writableFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be writable and not equal secretFile)
        }
        assert(caught4.message === Some(allError(wasWritable(writableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'all(paths) should be (writable)'") {

      it("should do nothing when all(paths) is writable") {
        all(List(writableFile.toPath)) should (be (writableFile.toPath) and be (writable))
        all(List(writableFile.toPath)) should (be (writable) and be (writableFile.toPath))

        all(List(writableFile.toPath)) should (equal (writableFile.toPath) and be (writable))
        all(List(writableFile.toPath)) should (be (writable) and equal (writableFile.toPath))
      }

      it("should throw TestFailedException with correct stack depth when all(pats) is not writable") {
        val left1 = List(secretFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (secretFile.toPath) and be (writable))
        }
        assert(caught1.message === Some(allError(wasEqualTo(secretFile.toPath, secretFile.toPath) + ", but " + wasNotWritable(secretFile.toPath), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left2 = List(secretFile.toPath)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (writable) and be (secretFile.toPath))
        }
        assert(caught2.message === Some(allError(wasNotWritable(secretFile.toPath), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left3 = List(secretFile.toPath)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (secretFile.toPath) and be (writable))
        }
        assert(caught3.message === Some(allError(equaled(secretFile.toPath, secretFile.toPath) + ", but " + wasNotWritable(secretFile.toPath), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left4 = List(secretFile.toPath)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (writable) and equal (secretFile.toPath))
        }
        assert(caught4.message === Some(allError(wasNotWritable(secretFile.toPath), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'all(paths) should not be writable'") {
      
      it("should do nothing when all(paths) is not writable") {
        all(List(secretFile.toPath)) should (not be writable and not be writableFile.toPath)
        all(List(secretFile.toPath)) should (not be writableFile.toPath and not be writable)

        all(List(secretFile.toPath)) should (not be writable and not equal writableFile.toPath)
        all(List(secretFile.toPath)) should (not equal writableFile.toPath and not be writable)
      }

      it("should throw TestFailedException with correct stack depth when all(paths) is writable") {
        val left1 = List(writableFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be secretFile.toPath and not be writable)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(writableFile.toPath, secretFile.toPath) + ", but " + wasWritable(writableFile.toPath), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left2 = List(writableFile.toPath)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be writable and not be secretFile.toPath)
        }
        assert(caught2.message === Some(allError(wasWritable(writableFile.toPath), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left3 = List(writableFile.toPath)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal secretFile.toPath and not be writable)
        }
        assert(caught3.message === Some(allError(didNotEqual(writableFile.toPath, secretFile.toPath) + ", but " + wasWritable(writableFile.toPath), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

        val left4 = List(writableFile.toPath)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be writable and not equal secretFile.toPath)
        }
        assert(caught4.message === Some(allError(wasWritable(writableFile.toPath), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
