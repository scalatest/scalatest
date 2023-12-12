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

// SKIP-SCALATESTJS,NATIVE-START
import java.io.File
// SKIP-SCALATESTJS,NATIVE-END
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ShouldBeReadableSpec extends AnyFunSpec with Matchers {

  private val prettifier = Prettifier.default

  // SKIP-SCALATESTJS,NATIVE-START
  val tempDir = createTempDirectory()
  val readableFile = File.createTempFile("delete", "me", tempDir)
  readableFile.setReadable(true)
  
  val secretFile = new File(tempDir, "imaginary")
  secretFile.setReadable(false)
  // SKIP-SCALATESTJS,NATIVE-END

  //SCALATESTJS,NATIVE-ONLY trait File { def isReadable: Boolean }
  //SCALATESTJS,NATIVE-ONLY val readableFile = new File { val isReadable = true }
  //SCALATESTJS,NATIVE-ONLY val secretFile = new File { val isReadable = false }
  
  val fileName: String = "ShouldBeReadableSpec.scala"
    
  def wasNotReadable(left: Any): String = 
    FailureMessages.wasNotReadable(prettifier, left)
    
  def wasReadable(left: Any): String = 
    FailureMessages.wasReadable(prettifier, left)
  
  it("readableFile should be readable, secretFile should not be readable") {
    // SKIP-SCALATESTJS,NATIVE-START
    assert(readableFile.canRead === true)
    assert(secretFile.canRead === false)
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY assert(readableFile.isReadable === true)
    //SCALATESTJS,NATIVE-ONLY assert(secretFile.isReadable === false)
  }

  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }

  describe("Readable matcher") {

    describe("when work with 'file should be (readable)'") {

      it("should do nothing when file is readable") {
        readableFile should be (readable)
      }

      it("should throw TestFailedException with correct stack depth when file is not readable") {
        val caught1 = intercept[TestFailedException] {
          secretFile should be (readable)
        }
        assert(caught1.message === Some(wasNotReadable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'file should not be readable'") {

      it("should do nothing when file is not readable") {
        secretFile should not be readable
      }

      it("should throw TestFailedException with correct stack depth when file is readable") {
        val caught1 = intercept[TestFailedException] {
          readableFile should not be readable
        }
        assert(caught1.message === Some(wasReadable(readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'file shouldBe readable'") {

      it("should do nothing when file is readable") {
        readableFile shouldBe readable
      }

      it("should throw TestFailedException with correct stack depth when file is not readable") {
        val caught1 = intercept[TestFailedException] {
          secretFile shouldBe readable
        }
        assert(caught1.message === Some(wasNotReadable(secretFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'file shouldNot be (readable)'") {

      it("should do nothing when file is not readable") {
        secretFile shouldNot be (readable)
      }

      it("should throw TestFailedException with correct stack depth when file is readable") {
        val caught1 = intercept[TestFailedException] {
          readableFile shouldNot be (readable)
        }
        assert(caught1.message === Some(wasReadable(readableFile)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'all(xs) should be (readable)'") {

      it("should do nothing when all(xs) is readable") {
        all(List(readableFile)) should be (readable)
      }

      it("should throw TestFailedException with correct stack depth when all(xs) is not readable") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (readable)
        }
        assert(caught1.message === Some(allError(left1, wasNotReadable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'all(xs) should not be readable'") {

      it("should do nothing when all(xs) is not readable") {
        all(List(secretFile)) should not be readable
      }

      it("should throw TestFailedException with correct stack depth when all(xs) is readable") {
        val left1 = List(readableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be readable
        }
        assert(caught1.message === Some(allError(left1, wasReadable(readableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

    }

    describe("when work with 'all(xs) shouldBe readable'") {

      it("should do nothing when all(xs) is readable") {
        all(List(readableFile)) shouldBe readable
      }

      it("should throw TestFailedException with correct stack depth when all(xs) is not readable") {
        val left1 = List(secretFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe readable
        }
        assert(caught1.message === Some(allError(left1, wasNotReadable(secretFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("when work with 'all(xs) shouldNot be (readable)'") {

      it("should do nothing when all(xs) is not readable") {
        all(List(secretFile)) shouldNot be (readable)
      }

      it("should throw TestFailedException with correct stack depth when all(xs) is readable") {
        val left1 = List(readableFile)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (readable)
        }
        assert(caught1.message === Some(allError(left1, wasReadable(readableFile), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
