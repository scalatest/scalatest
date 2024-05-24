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
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldExistLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  // SKIP-SCALATESTJS,NATIVE-START
  val tempDir = createTempDirectory()
  val existFile = File.createTempFile("delete", "me", tempDir)
  val imaginaryFile = new File(tempDir, "imaginary")
  // SKIP-SCALATESTJS,NATIVE-END
  //SCALATESTJS,NATIVE-ONLY trait File { def exists: Boolean }
  //SCALATESTJS,NATIVE-ONLY val existFile = new File { val exists: Boolean = true }
  //SCALATESTJS,NATIVE-ONLY val imaginaryFile = new File { val exists: Boolean = false }
  //SCALATESTJS,NATIVE-ONLY implicit val fileExistence: org.scalatest.enablers.Existence[File] = new org.scalatest.enablers.Existence[File] { def exists(file: File): Boolean = file.exists }

  val fileName = "ShouldExistLogicalAndSpec.scala"
  
  def doesNotExist(left: Any): String = 
    FailureMessages.doesNotExist(prettifier, left)
    
  def exists(left: Any): String = 
    FailureMessages.exists(prettifier, left)
    
  def wasEqualTo(left: Any, right: Any): String =
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotEqualTo(prettifier, left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
  
  def didNotEqual(left: Any, right: Any): String = 
    FailureMessages.didNotEqual(prettifier, left, right)
    
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
    
  describe("The exist syntax when used with File") {
    
    it("should do nothing when the file exists") {
      existFile should (equal (existFile) and exist)
      existFile should (exist and equal (existFile))
      existFile should (be (existFile) and exist)
      existFile should (exist and be (existFile))
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val e1 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (equal (imaginaryFile) and exist)
      }
      assert(e1.message === Some(equaled(imaginaryFile, imaginaryFile) + ", but " + doesNotExist(imaginaryFile)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        existFile should (exist and equal (imaginaryFile))
      }
      assert(e2.message === Some(exists(existFile) + ", but " + didNotEqual(existFile, imaginaryFile)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (be (imaginaryFile) and exist)
      }
      assert(e3.message === Some(wasEqualTo(imaginaryFile, imaginaryFile) + ", but " + doesNotExist(imaginaryFile)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        existFile should (exist and be (imaginaryFile))
      }
      assert(e4.message === Some(exists(existFile) + ", but " + wasNotEqualTo(existFile, imaginaryFile)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      imaginaryFile should (equal (imaginaryFile) and not (exist))
      imaginaryFile should (not (exist) and equal (imaginaryFile))
      imaginaryFile should (be (imaginaryFile) and not (exist))
      imaginaryFile should (not (exist) and be (imaginaryFile))
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val e1 = intercept[exceptions.TestFailedException] {
        existFile should (equal (existFile) and not (exist))
      }
      assert(e1.message === Some(equaled(existFile, existFile) + ", but " + exists(existFile)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (not (exist) and equal (existFile))
      }
      assert(e2.message === Some(doesNotExist(imaginaryFile) + ", but " + didNotEqual(imaginaryFile, existFile)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        existFile should (be (existFile) and not (exist))
      }
      assert(e3.message === Some(wasEqualTo(existFile, existFile) + ", but " + exists(existFile)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (not (exist) and be (existFile))
      }
      assert(e4.message === Some(doesNotExist(imaginaryFile) + ", but " + wasNotEqualTo(imaginaryFile, existFile)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  describe("The exist syntax when used with all(xs)") {
    
    it("should do nothing when the file exists") {
      all(List(existFile)) should (equal (existFile) and exist)
      all(List(existFile)) should (exist and equal (existFile))
      all(List(existFile)) should (be (existFile) and exist)
      all(List(existFile)) should (exist and be (existFile))
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val left1 = List(imaginaryFile)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (imaginaryFile) and exist)
      }
      assert(e1.message === Some(allError(left1, equaled(imaginaryFile, imaginaryFile) + ", but " + doesNotExist(imaginaryFile), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(existFile)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (exist and equal (imaginaryFile))
      }
      assert(e2.message === Some(allError(left2, exists(existFile) + ", but " + didNotEqual(existFile, imaginaryFile), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(imaginaryFile)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (imaginaryFile) and exist)
      }
      assert(e3.message === Some(allError(left3, wasEqualTo(imaginaryFile, imaginaryFile) + ", but " + doesNotExist(imaginaryFile), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(existFile)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (exist and be (imaginaryFile))
      }
      assert(e4.message === Some(allError(left4, exists(existFile) + ", but " + wasNotEqualTo(existFile, imaginaryFile), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      all(List(imaginaryFile)) should (equal (imaginaryFile) and not (exist))
      all(List(imaginaryFile)) should (not (exist) and equal (imaginaryFile))
      all(List(imaginaryFile)) should (be (imaginaryFile) and not (exist))
      all(List(imaginaryFile)) should (not (exist) and be (imaginaryFile))
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val left1 = List(existFile)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (existFile) and not (exist))
      }
      assert(e1.message === Some(allError(left1, equaled(existFile, existFile) + ", but " + exists(existFile), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(imaginaryFile)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (not (exist) and equal (existFile))
      }
      assert(e2.message === Some(allError(left2, doesNotExist(imaginaryFile) + ", but " + didNotEqual(imaginaryFile, existFile), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(existFile)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (existFile) and not (exist))
      }
      assert(e3.message === Some(allError(left3, wasEqualTo(existFile, existFile) + ", but " + exists(existFile), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(imaginaryFile)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (not (exist) and be (existFile))
      }
      assert(e4.message === Some(allError(left4, doesNotExist(imaginaryFile) + ", but " + wasNotEqualTo(imaginaryFile, existFile), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
  }
}