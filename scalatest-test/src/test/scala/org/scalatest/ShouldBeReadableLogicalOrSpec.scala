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

class ShouldBeReadableLogicalOrSpec extends Spec {
  
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
    
  val tempDir = createTempDirectory()
  val readableFile = File.createTempFile("delete", "me", tempDir)
  readableFile.setReadable(true)
  
  val secretFile = new File(tempDir, "imaginary")
  secretFile.setReadable(false)
  
  object `Sorted matcher` {
    
    object `when work with 'file should be (readable)'` {
      
      def `should do nothing when file is readable` {
        
        readableFile should (be (readable) or be_== (readableFile))
        secretFile should (be (readable) or be_== (secretFile))
        readableFile should (be (readable) or be_== (secretFile))
        
        readableFile should (be_== (readableFile) or be (readable))
        readableFile should (be_== (secretFile) or be (readable))
        secretFile should (be_== (secretFile) or be (readable))
        
        readableFile should (be (readable) or equal (readableFile))
        secretFile should (be (readable) or equal (secretFile))
        readableFile should (be (readable) or equal (secretFile))
        
        readableFile should (equal (readableFile) or be (readable))
        readableFile should (equal (secretFile) or be (readable))
        secretFile should (equal (secretFile) or be (readable))
      }
      
      def `should throw TestFailedException with correct stack depth when file is not readable` {
        val caught1 = intercept[TestFailedException] {
          secretFile should (be (readable) or be_== (readableFile))
        }
        assert(caught1.message === Some(wasNotReadable(secretFile) + ", and " + wasNotEqualTo(secretFile, readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          secretFile should (be_== (readableFile) or be (readable))
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
    
    object `when work with 'file should not be readable'` {
      
      def `should do nothing when file is not readable` {
        secretFile should (not be readable or not be_== readableFile)
        readableFile should (not be readable or not be_== secretFile)
        secretFile should (not be readable or not be_== secretFile)
        
        secretFile should (not be_== readableFile or not be readable)
        secretFile should (not be_== secretFile or not be readable)
        readableFile should (not be_== secretFile or not be readable)
        
        secretFile should (not be readable or not equal readableFile)
        readableFile should (not be readable or not equal secretFile)
        secretFile should (not be readable or not equal secretFile)
        
        secretFile should (not equal readableFile or not be readable)
        secretFile should (not equal secretFile or not be readable)
        readableFile should (not equal secretFile or not be readable)
      }
      
      def `should throw TestFailedException with correct stack depth when file is readable` {
        val caught1 = intercept[TestFailedException] {
          readableFile should (not be readable or not be_== readableFile)
        }
        assert(caught1.message === Some(wasReadable(readableFile) + ", and " + wasEqualTo(readableFile, readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          readableFile should (not be_== readableFile or not be readable)
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
    
    object `when work with 'all(xs) should be (readable)'` {
      
      def `should do nothing when all(xs) is readable` {
        all(List(readableFile)) should (be (readable) or be_== (readableFile))
        all(List(secretFile)) should (be (readable) or be_== (secretFile))
        all(List(readableFile)) should (be (readable) or be_== (secretFile))
        
        all(List(readableFile)) should (be_== (readableFile) or be (readable))
        all(List(readableFile)) should (be_== (secretFile) or be (readable))
        all(List(secretFile)) should (be_== (secretFile) or be (readable))
        
        all(List(readableFile)) should (be (readable) or equal (readableFile))
        all(List(secretFile)) should (be (readable) or equal (secretFile))
        all(List(readableFile)) should (be (readable) or equal (secretFile))
        
        all(List(readableFile)) should (equal (readableFile) or be (readable))
        all(List(readableFile)) should (equal (secretFile) or be (readable))
        all(List(secretFile)) should (equal (secretFile) or be (readable))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be_== (readableFile) or be (readable))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(secretFile, readableFile) + ", and " + wasNotReadable(secretFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(secretFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (readable) or be_== (readableFile))
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
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(secretFile)) should (not be readable or not be_== readableFile)
        all(List(readableFile)) should (not be readable or not be_== secretFile)
        all(List(secretFile)) should (not be readable or not be_== secretFile)
        
        all(List(secretFile)) should (not be_== readableFile or not be readable)
        all(List(secretFile)) should (not be_== secretFile or not be readable)
        all(List(readableFile)) should (not be_== secretFile or not be readable)
        
        all(List(secretFile)) should (not be readable or not equal readableFile)
        all(List(readableFile)) should (not be readable or not equal secretFile)
        all(List(secretFile)) should (not be readable or not equal secretFile)
        
        all(List(secretFile)) should (not equal readableFile or not be readable)
        all(List(secretFile)) should (not equal secretFile or not be readable)
        all(List(readableFile)) should (not equal secretFile or not be readable)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(readableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be_== readableFile or not be readable)
        }
        assert(caught1.message === Some(allError(wasEqualTo(readableFile, readableFile) + ", and " + wasReadable(readableFile), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(readableFile)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be readable or not be_== readableFile)
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
