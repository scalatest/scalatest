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
import org.scalactic.Prettifier

class ShouldBeWritableSpec extends FunSpec {

  private val prettifier = Prettifier.default

  // SKIP-SCALATESTJS-START
  val tempDir = createTempDirectory()
  val writableFile = File.createTempFile("writable", "me", tempDir)
  writableFile.setWritable(true)
  
  val secretFile = new File(tempDir, "secret")
  secretFile.setWritable(false)
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY trait File { def canWrite: Boolean; def isWritable: Boolean = canWrite }
  //SCALATESTJS-ONLY val writableFile = new File { val canWrite: Boolean = true }
  //SCALATESTJS-ONLY val secretFile = new File { val canWrite: Boolean = false }
  
  val fileName: String = "ShouldBeWritableSpec.scala"
    
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(prettifier, left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(prettifier, left)
  
  it("writableFile should be writable, secretFile should not be writable") {
    assert(writableFile.canWrite === true)
    assert(secretFile.canWrite === false)
  }
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
  
  describe("writable matcher") {
    
    describe("when work with 'file should be (writable)'") {
      
      it("should do nothing when file is writable") {
        writableFile should be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'file should not be writable'") {
      
      it("should do nothing when file is not writable") {
        secretFile should not be writable
      }
      
      it("should throw TestFailedException with correct stack depth when file is writable") {
        val caught1 = intercept[TestFailedException] {
          writableFile should not be writable
        }
        assert(caught1.message === Some(wasWritable(writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file shouldBe writable'") {
      
      it("should do nothing when file is writable") {
        writableFile shouldBe writable
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile shouldBe writable
        }
        assert(caught1.message === Some(wasNotWritable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'file shouldNot be (writable)'") {
      
      it("should do nothing when file is not writable") {
        secretFile shouldNot be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is writable") {
        val caught1 = intercept[TestFailedException] {
          writableFile shouldNot be (writable)
        }
        assert(caught1.message === Some(wasWritable(writableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }

    describe("when work with 'path should be (writable)'") {

      it("should do nothing when path is writable") {
        writableFile.toPath should be (writable)
      }

      it("should throw TestFailedException with correct stack depth when path is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile.toPath should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(secretFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'path should not be writable'") {

      it("should do nothing when path is not writable") {
        secretFile.toPath should not be writable
      }

      it("should throw TestFailedException with correct stack depth when path is writable") {
        val caught1 = intercept[TestFailedException] {
          writableFile.toPath should not be writable
        }
        assert(caught1.message === Some(wasWritable(writableFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'path shouldBe writable'") {

      it("should do nothing when path is writable") {
        writableFile.toPath shouldBe writable
      }

      it("should throw TestFailedException with correct stack depth when path is not writable") {
        val caught1 = intercept[TestFailedException] {
          secretFile.toPath shouldBe writable
        }
        assert(caught1.message === Some(wasNotWritable(secretFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'path shouldNot be (writable)'") {

      it("should do nothing when path is not writable") {
        secretFile.toPath shouldNot be (writable)
      }

      it("should throw TestFailedException with correct stack depth when path is writable") {
        val caught1 = intercept[TestFailedException] {
          writableFile.toPath shouldNot be (writable)
        }
        assert(caught1.message === Some(wasWritable(writableFile.toPath)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }
    
    describe("when work with 'all(xs) should be (writable)'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(writableFile)) should be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not writable") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should not be writable'") {
      
      it("should do nothing when all(xs) is not writable") {
        all(List(secretFile)) should not be writable
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is writable") {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be writable
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) shouldBe writable'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(writableFile)) shouldBe writable
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not writable") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe writable
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) shouldNot be (writable)'") {
      
      it("should do nothing when all(xs) is not writable") {
        all(List(secretFile)) shouldNot be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is writable") {
        val left1 = List(writableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }

    describe("when work with 'all(paths) should be (writable)'") {

      it("should do nothing when all(paths) is writable") {
        all(List(writableFile.toPath)) should be (writable)
      }

      it("should throw TestFailedException with correct stack depth when all(paths) is not writable") {
        val left1 = List(secretFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile.toPath), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'all(paths) should not be writable'") {

      it("should do nothing when all(paths) is not writable") {
        all(List(secretFile.toPath)) should not be writable
      }

      it("should throw TestFailedException with correct stack depth when all(paths) is writable") {
        val left1 = List(writableFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be writable
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile.toPath), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'all(paths) shouldBe writable'") {

      it("should do nothing when all(paths) is writable") {
        all(List(writableFile.toPath)) shouldBe writable
      }

      it("should throw TestFailedException with correct stack depth when all(paths) is not writable") {
        val left1 = List(secretFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe writable
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(secretFile.toPath), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'all(paths) shouldNot be (writable)'") {

      it("should do nothing when all(paths) is not writable") {
        all(List(secretFile.toPath)) shouldNot be (writable)
      }

      it("should throw TestFailedException with correct stack depth when all(paths) is writable") {
        val left1 = List(writableFile.toPath)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(writableFile.toPath), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }
  }
  
}
