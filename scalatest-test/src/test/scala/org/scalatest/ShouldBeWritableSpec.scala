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

import SharedHelpers.{createTempDirectory, thisLineNumber}
import java.io.File
import Matchers._
import exceptions.TestFailedException

class ShouldBeWritableSpec extends Spec {
  
  val tempDir = createTempDirectory()
  val writableFile = File.createTempFile("writable", "me", tempDir)
  writableFile.setWritable(true)
  
  val secretFile = new File(tempDir, "secret")
  secretFile.setWritable(false)
  
  val fileName: String = "ShouldBeWritableSpec.scala"
    
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(left)
  
  def `writableFile should be writable, secretFile should not be writable` {
    assert(writableFile.canRead === true)
    assert(secretFile.canRead === false)
  }
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
  
  object `writable matcher` {
    
    object `when work with 'file should be (writable)'` {
      
      def `should do nothing when file is writable` {
        writableFile should be (writable)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          secretFile should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file should not be writable'` {
      
      def `should do nothing when file is not writable` {
        secretFile should not be writable
      }
      
      def `should throw TestFailedException with correct stack depth when file is writable` {
        val caught1 = intercept[TestFailedException] {
          writableFile should not be writable
        }
        assert(caught1.message === Some(wasWritable(writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file shouldBe writable'` {
      
      def `should do nothing when file is writable` {
        writableFile shouldBe writable
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          secretFile shouldBe writable
        }
        assert(caught1.message === Some(wasNotWritable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file shouldNot be (writable)'` {
      
      def `should do nothing when file is not writable` {
        secretFile shouldNot be (writable)
      }
      
      def `should throw TestFailedException with correct stack depth when file is writable` {
        val caught1 = intercept[TestFailedException] {
          writableFile shouldNot be (writable)
        }
        assert(caught1.message === Some(wasWritable(writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should be (writable)'` {
      
      def `should do nothing when all(xs) is writable` {
        all(List(writableFile)) should be (writable)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not writable` {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should not be writable'` {
      
      def `should do nothing when all(xs) is not writable` {
        all(List(secretFile)) should not be writable
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is writable` {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be writable
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) shouldBe writable'` {
      
      def `should do nothing when all(xs) is writable` {
        all(List(writableFile)) shouldBe writable
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not writable` {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe writable
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) shouldNot be (writable)'` {
      
      def `should do nothing when all(xs) is not writable` {
        all(List(secretFile)) shouldNot be (writable)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is writable` {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
