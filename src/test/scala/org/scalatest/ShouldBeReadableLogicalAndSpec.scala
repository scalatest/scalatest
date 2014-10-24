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

class ShouldBeReadableLogicalAndSpec extends Spec {
  
  val fileName: String = "ShouldBeReadableLogicalAndSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotReadable(left: Any): String = 
    FailureMessages("wasNotReadable", left)
    
  def wasReadable(left: Any): String = 
    FailureMessages("wasReadable", left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
    
  val tempDir = createTempDirectory()
  val readableFile = File.createTempFile("delete", "me", tempDir)
  readableFile.setReadable(true)
  
  val secretFile = new File(tempDir, "imaginary")
  secretFile.setReadable(false)
  
  object `Readability matcher` {
    
    object `when work with 'file should be (readable)'` {
      
      def `should do nothing when file is readable` {
        readableFile should (equal (readableFile) and be (readable))
        readableFile should (be (readable) and equal (readableFile))
        
        readableFile should (be_== (readableFile) and be (readable))
        readableFile should (be (readable) and be_== (readableFile))
      }
      
      def `should throw TestFailedException with correct stack depth when file is not readable` {
        val caught1 = intercept[TestFailedException] {
          secretFile should (equal (secretFile) and be (readable))
        }
        assert(caught1.message === Some(equaled(secretFile, secretFile) + ", but " + wasNotReadable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          secretFile should (be (readable) and equal (secretFile))
        }
        assert(caught2.message === Some(wasNotReadable(secretFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          secretFile should (be_== (secretFile) and be (readable))
        }
        assert(caught3.message === Some(wasEqualTo(secretFile, secretFile) + ", but " + wasNotReadable(secretFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          secretFile should (be (readable) and be_== (secretFile))
        }
        assert(caught4.message === Some(wasNotReadable(secretFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be sorted'` {
      
      def `should do nothing when file is not readable` {
        secretFile should (not equal readableFile and not be readable)
        secretFile should (not be readable and not equal readableFile)
        
        secretFile should (not be_== readableFile and not be readable)
        secretFile should (not be readable and not be_== readableFile)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          readableFile should (not equal secretFile and not be readable)
        }
        assert(caught1.message === Some(didNotEqual(readableFile, secretFile) + ", but " + wasReadable(readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          readableFile should (not be readable and not equal secretFile)
        }
        assert(caught2.message === Some(wasReadable(readableFile)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          readableFile should (not be_== secretFile and not be readable)
        }
        assert(caught3.message === Some(wasNotEqualTo(readableFile, secretFile) + ", but " + wasReadable(readableFile)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          readableFile should (not be readable and not be_== secretFile)
        }
        assert(caught4.message === Some(wasReadable(readableFile)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (readable)'` {
      
      def `should do nothing when all(xs) is readable` {
        all(List(readableFile)) should (be_== (readableFile) and be (readable))
        all(List(readableFile)) should (be (readable) and be_== (readableFile))
        
        all(List(readableFile)) should (equal (readableFile) and be (readable))
        all(List(readableFile)) should (be (readable) and equal (readableFile))
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not readable` {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be_== (secretFile) and be (readable))
        }
        assert(caught1.message === Some(allError(wasEqualTo(secretFile, secretFile) + ", but " + wasNotReadable(secretFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(secretFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (readable) and be_== (secretFile))
        }
        assert(caught2.message === Some(allError(wasNotReadable(secretFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(secretFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (secretFile) and be (readable))
        }
        assert(caught3.message === Some(allError(equaled(secretFile, secretFile) + ", but " + wasNotReadable(secretFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(secretFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (readable) and equal (secretFile))
        }
        assert(caught4.message === Some(allError(wasNotReadable(secretFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be readable'` {
      def `should do nothing when all(xs) is not readable` {
        all(List(secretFile)) should (not be readable and not be_== readableFile)
        all(List(secretFile)) should (not be_== readableFile and not be readable)
        
        all(List(secretFile)) should (not be readable and not equal readableFile)
        all(List(secretFile)) should (not equal readableFile and not be readable)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is readable` {
        val left1 = List(readableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be_== secretFile and not be readable)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(readableFile, secretFile) + ", but " + wasReadable(readableFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(readableFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be readable and not be_== secretFile)
        }
        assert(caught2.message === Some(allError(wasReadable(readableFile), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(readableFile)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal secretFile and not be readable)
        }
        assert(caught3.message === Some(allError(didNotEqual(readableFile, secretFile) + ", but " + wasReadable(readableFile), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(readableFile)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be readable and not equal secretFile)
        }
        assert(caught4.message === Some(allError(wasReadable(readableFile), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
