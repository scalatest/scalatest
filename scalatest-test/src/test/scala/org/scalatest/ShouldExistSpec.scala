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

import java.io.File
import SharedHelpers.{createTempDirectory, thisLineNumber}
import Matchers._

class ShouldExistSpec extends FunSpec {

  // SKIP-SCALATESTJS-START
  val tempDir = createTempDirectory()
  val existFile = File.createTempFile("delete", "me", tempDir)
  val imaginaryFile = new File(tempDir, "imaginary")
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY trait File { def exists: Boolean }
  //SCALATESTJS-ONLY val existFile = new File { val exists: Boolean = true }
  //SCALATESTJS-ONLY val imaginaryFile = new File { val exists: Boolean = false }
  //SCALATESTJS-ONLY implicit val fileExistence = new org.scalatest.enablers.Existence[File] { def exists(file: File): Boolean = file.exists }
  
  val fileName = "ShouldExistSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages.doesNotExist(left)
    
  def exists(left: Any): String = 
    FailureMessages.exists(left)
    
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
  
  describe("The exist syntax when used with File") {
    
    it("should do nothing when the file exists") {
      existFile should exist
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val e = intercept[exceptions.TestFailedException] {
        imaginaryFile should exist
      }
      assert(e.message === Some(doesNotExist(imaginaryFile)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      imaginaryFile should not (exist)
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val e = intercept[exceptions.TestFailedException] {
        existFile should not (exist)
      }
      assert(e.message === Some(exists(existFile)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with shouldNot and the file does not exists") {
      imaginaryFile shouldNot exist
    }
    
    it("should throw TFE with correct stack depth and message when it is used with shouldNot and  the file exists") {
      val e = intercept[exceptions.TestFailedException] {
        existFile shouldNot exist
      }
      assert(e.message === Some(exists(existFile)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  describe("The exist syntax when used with all(xs)") {
    
    it("should do nothing when the file exists") {
      all(List(existFile)) should exist
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val left = List(imaginaryFile)
      val e = intercept[exceptions.TestFailedException] {
        all(left) should exist
      }
      assert(e.message === Some(allError(left, doesNotExist(imaginaryFile), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      all(List(imaginaryFile)) should not (exist)
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val left = List(existFile)
      val e = intercept[exceptions.TestFailedException] {
        all(left) should not (exist)
      }
      assert(e.message === Some(allError(left, exists(existFile), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with shouldNot and the file does not exists") {
      all(List(imaginaryFile)) shouldNot exist
    }
    
    it("should throw TFE with correct stack depth and message when it is used with shouldNot and  the file exists") {
      val left = List(existFile)
      val e = intercept[exceptions.TestFailedException] {
        all(left) shouldNot exist
      }
      assert(e.message === Some(allError(left, exists(existFile), thisLineNumber - 2)))
      assert(e.failedCodeFileName === Some(fileName))
      assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
}