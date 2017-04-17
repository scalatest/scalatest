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
import org.scalactic.Prettifier

class ShouldExistLogicalOrSpec extends FunSpec {

  private val prettifier = Prettifier.default

  // SKIP-SCALATESTJS-START
  val tempDir = createTempDirectory()
  val existFile = File.createTempFile("delete", "me", tempDir)
  val imaginaryFile = new File(tempDir, "imaginary")
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY trait File { def exists: Boolean }
  //SCALATESTJS-ONLY val existFile = new File { val exists: Boolean = true }
  //SCALATESTJS-ONLY val imaginaryFile = new File { val exists: Boolean = false }
  //SCALATESTJS-ONLY implicit val fileExistence = new org.scalatest.enablers.Existence[File] { def exists(file: File): Boolean = file.exists }
  
  val fileName = "ShouldExistLogicalOrSpec.scala"
  
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
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
    
  describe("The exist syntax when used with File") {
    
    it("should do nothing when the file exists") {
      existFile should (equal (existFile) or exist)
      existFile should (equal (imaginaryFile) or exist)
      imaginaryFile should (equal (imaginaryFile) or exist)
      
      existFile should (exist or equal (existFile))
      imaginaryFile should (exist or equal (imaginaryFile))
      existFile should (exist or equal (imaginaryFile))
      
      existFile should (be (existFile) or exist)
      existFile should (be (imaginaryFile) or exist)
      imaginaryFile should (be (imaginaryFile) or exist)
      
      existFile should (exist or be (existFile))
      imaginaryFile should (exist or be (imaginaryFile))
      existFile should (exist or be (imaginaryFile))
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val e1 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (equal (existFile) or exist)
      }
      assert(e1.message === Some(didNotEqual(imaginaryFile, existFile) + ", and " + doesNotExist(imaginaryFile)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (exist or equal (existFile))
      }
      assert(e2.message === Some(doesNotExist(imaginaryFile) + ", and " + didNotEqual(imaginaryFile, existFile)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (be (existFile) or exist)
      }
      assert(e3.message === Some(wasNotEqualTo(imaginaryFile, existFile) + ", and " + doesNotExist(imaginaryFile)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (exist or be (existFile))
      }
      assert(e4.message === Some(doesNotExist(imaginaryFile) + ", and " + wasNotEqualTo(imaginaryFile, existFile)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      imaginaryFile should (equal (imaginaryFile) or not (exist))
      imaginaryFile should (equal (existFile) or not (exist))
      existFile should (equal (existFile) or not (exist))
      
      imaginaryFile should (not (exist) or equal (imaginaryFile))
      existFile should (not (exist) or equal (existFile))
      imaginaryFile should (not (exist) or equal (existFile))
      
      imaginaryFile should (be (imaginaryFile) or not (exist))
      imaginaryFile should (be (existFile) or not (exist))
      existFile should (be (existFile) or not (exist))
      
      imaginaryFile should (not (exist) or be (imaginaryFile))
      existFile should (not (exist) or be (existFile))
      imaginaryFile should (not (exist) or be (existFile))
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val e1 = intercept[exceptions.TestFailedException] {
        existFile should (equal (imaginaryFile) or not (exist))
      }
      assert(e1.message === Some(didNotEqual(existFile, imaginaryFile) + ", and " + exists(existFile)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e2 = intercept[exceptions.TestFailedException] {
        existFile should (not (exist) or equal (imaginaryFile))
      }
      assert(e2.message === Some(exists(existFile) + ", and " + didNotEqual(existFile, imaginaryFile)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e3 = intercept[exceptions.TestFailedException] {
        existFile should (be (imaginaryFile) or not (exist))
      }
      assert(e3.message === Some(wasNotEqualTo(existFile, imaginaryFile) + ", and " + exists(existFile)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val e4 = intercept[exceptions.TestFailedException] {
        existFile should (not (exist) or be (imaginaryFile))
      }
      assert(e4.message === Some(exists(existFile) + ", and " + wasNotEqualTo(existFile, imaginaryFile)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }

  describe("The exist syntax when used with Path") {

    it("should do nothing when the path exists") {
      existFile.toPath should (equal (existFile.toPath) or exist)
      existFile.toPath should (equal (imaginaryFile.toPath) or exist)
      imaginaryFile.toPath should (equal (imaginaryFile.toPath) or exist)

      existFile.toPath should (exist or equal (existFile.toPath))
      imaginaryFile.toPath should (exist or equal (imaginaryFile.toPath))
      existFile.toPath should (exist or equal (imaginaryFile.toPath))

      existFile.toPath should (be (existFile.toPath) or exist)
      existFile.toPath should (be (imaginaryFile.toPath) or exist)
      imaginaryFile.toPath should (be (imaginaryFile.toPath) or exist)

      existFile.toPath should (exist or be (existFile.toPath))
      imaginaryFile.toPath should (exist or be (imaginaryFile.toPath))
      existFile.toPath should (exist or be (imaginaryFile.toPath))
    }

    it("should throw TFE with correct stack depth and message when the path does not exist") {
      val e1 = intercept[exceptions.TestFailedException] {
        imaginaryFile.toPath should (equal (existFile.toPath) or exist)
      }
      assert(e1.message === Some(didNotEqual(imaginaryFile.toPath, existFile.toPath) + ", and " + doesNotExist(imaginaryFile.toPath)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e2 = intercept[exceptions.TestFailedException] {
        imaginaryFile.toPath should (exist or equal (existFile.toPath))
      }
      assert(e2.message === Some(doesNotExist(imaginaryFile.toPath) + ", and " + didNotEqual(imaginaryFile.toPath, existFile.toPath)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e3 = intercept[exceptions.TestFailedException] {
        imaginaryFile should (be (existFile) or exist)
      }
      assert(e3.message === Some(wasNotEqualTo(imaginaryFile.toPath, existFile.toPath) + ", and " + doesNotExist(imaginaryFile.toPath)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e4 = intercept[exceptions.TestFailedException] {
        imaginaryFile.toPath should (exist or be (existFile.toPath))
      }
      assert(e4.message === Some(doesNotExist(imaginaryFile.toPath) + ", and " + wasNotEqualTo(imaginaryFile.toPath, existFile.toPath)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should do nothing when it is used with not and the path does not exists") {
      imaginaryFile.toPath should (equal (imaginaryFile.toPath) or not (exist))
      imaginaryFile.toPath should (equal (existFile.toPath) or not (exist))
      existFile.toPath should (equal (existFile.toPath) or not (exist))

      imaginaryFile.toPath should (not (exist) or equal (imaginaryFile.toPath))
      existFile.toPath should (not (exist) or equal (existFile.toPath))
      imaginaryFile.toPath should (not (exist) or equal (existFile.toPath))

      imaginaryFile.toPath should (be (imaginaryFile.toPath) or not (exist))
      imaginaryFile.toPath should (be (existFile.toPath) or not (exist))
      existFile.toPath should (be (existFile.toPath) or not (exist))

      imaginaryFile.toPath should (not (exist) or be (imaginaryFile.toPath))
      existFile.toPath should (not (exist) or be (existFile.toPath))
      imaginaryFile.toPath should (not (exist) or be (existFile.toPath))
    }

    it("should throw TFE with correct stack depth and message when it is used with not and the path exists") {
      val e1 = intercept[exceptions.TestFailedException] {
        existFile.toPath should (equal (imaginaryFile.toPath) or not (exist))
      }
      assert(e1.message === Some(didNotEqual(existFile.toPath, imaginaryFile.toPath) + ", and " + exists(existFile.toPath)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e2 = intercept[exceptions.TestFailedException] {
        existFile.toPath should (not (exist) or equal (imaginaryFile.toPath))
      }
      assert(e2.message === Some(exists(existFile.toPath) + ", and " + didNotEqual(existFile.toPath, imaginaryFile.toPath)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e3 = intercept[exceptions.TestFailedException] {
        existFile.toPath should (be (imaginaryFile.toPath) or not (exist))
      }
      assert(e3.message === Some(wasNotEqualTo(existFile.toPath, imaginaryFile.toPath) + ", and " + exists(existFile.toPath)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val e4 = intercept[exceptions.TestFailedException] {
        existFile.toPath should (not (exist) or be (imaginaryFile.toPath))
      }
      assert(e4.message === Some(exists(existFile.toPath) + ", and " + wasNotEqualTo(existFile.toPath, imaginaryFile.toPath)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
  describe("The exist syntax when used with all(xs)") {
    
    it("should do nothing when the file exists") {
      all(List(existFile)) should (equal (existFile) or exist)
      all(List(existFile)) should (equal (imaginaryFile) or exist)
      all(List(imaginaryFile)) should (equal (imaginaryFile) or exist)
      
      all(List(existFile)) should (exist or equal (existFile))
      all(List(imaginaryFile)) should (exist or equal (imaginaryFile))
      all(List(existFile)) should (exist or equal (imaginaryFile))
      
      all(List(existFile)) should (be (existFile) or exist)
      all(List(existFile)) should (be (imaginaryFile) or exist)
      all(List(imaginaryFile)) should (be (imaginaryFile) or exist)
      
      all(List(existFile)) should (exist or be (existFile))
      all(List(imaginaryFile)) should (exist or be (imaginaryFile))
      all(List(existFile)) should (exist or be (imaginaryFile))
    }
    
    it("should throw TFE with correct stack depth and message when the file does not exist") {
      val left1 = List(imaginaryFile)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (existFile) or exist)
      }
      assert(e1.message === Some(allError(left1, didNotEqual(imaginaryFile, existFile) + ", and " + doesNotExist(imaginaryFile), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(imaginaryFile)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (exist or equal (existFile))
      }
      assert(e2.message === Some(allError(left2, doesNotExist(imaginaryFile) + ", and " + didNotEqual(imaginaryFile, existFile), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(imaginaryFile)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (existFile) or exist)
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(imaginaryFile, existFile) + ", and " + doesNotExist(imaginaryFile), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(imaginaryFile)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (exist or be (existFile))
      }
      assert(e4.message === Some(allError(left4, doesNotExist(imaginaryFile) + ", and " + wasNotEqualTo(imaginaryFile, existFile), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should do nothing when it is used with not and the file does not exists") {
      all(List(imaginaryFile)) should (equal (imaginaryFile) or not (exist))
      all(List(imaginaryFile)) should (equal (existFile) or not (exist))
      all(List(existFile)) should (equal (existFile) or not (exist))
      
      all(List(imaginaryFile)) should (not (exist) or equal (imaginaryFile))
      all(List(existFile)) should (not (exist) or equal (existFile))
      all(List(imaginaryFile)) should (not (exist) or equal (existFile))
      
      all(List(imaginaryFile)) should (be (imaginaryFile) or not (exist))
      all(List(imaginaryFile)) should (be (existFile) or not (exist))
      all(List(existFile)) should (be (existFile) or not (exist))
      
      all(List(imaginaryFile)) should (not (exist) or be (imaginaryFile))
      all(List(existFile)) should (not (exist) or be (existFile))
      all(List(imaginaryFile)) should (not (exist) or be (existFile))
    }
    
    it("should throw TFE with correct stack depth and message when it is used with not and  the file exists") {
      val left1 = List(existFile)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (imaginaryFile) or not (exist))
      }
      assert(e1.message === Some(allError(left1, didNotEqual(existFile, imaginaryFile) + ", and " + exists(existFile), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left2 = List(existFile)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (not (exist) or equal (imaginaryFile))
      }
      assert(e2.message === Some(allError(left2, exists(existFile) + ", and " + didNotEqual(existFile, imaginaryFile), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left3 = List(existFile)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (imaginaryFile) or not (exist))
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(existFile, imaginaryFile) + ", and " + exists(existFile), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val left4 = List(existFile)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (not (exist) or be (imaginaryFile))
      }
      assert(e4.message === Some(allError(left4, exists(existFile) + ", and " + wasNotEqualTo(existFile, imaginaryFile), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
  }

  describe("The exist syntax when used with all(paths)") {

    it("should do nothing when the path exists") {
      all(List(existFile.toPath)) should (equal (existFile.toPath) or exist)
      all(List(existFile.toPath)) should (equal (imaginaryFile.toPath) or exist)
      all(List(imaginaryFile.toPath)) should (equal (imaginaryFile.toPath) or exist)

      all(List(existFile.toPath)) should (exist or equal (existFile.toPath))
      all(List(imaginaryFile.toPath)) should (exist or equal (imaginaryFile.toPath))
      all(List(existFile.toPath)) should (exist or equal (imaginaryFile.toPath))

      all(List(existFile.toPath)) should (be (existFile.toPath) or exist)
      all(List(existFile.toPath)) should (be (imaginaryFile.toPath) or exist)
      all(List(imaginaryFile.toPath)) should (be (imaginaryFile.toPath) or exist)

      all(List(existFile.toPath)) should (exist or be (existFile.toPath))
      all(List(imaginaryFile.toPath)) should (exist or be (imaginaryFile.toPath))
      all(List(existFile.toPath)) should (exist or be (imaginaryFile.toPath))
    }

    it("should throw TFE with correct stack depth and message when the path does not exist") {
      val left1 = List(imaginaryFile.toPath)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (existFile.toPath) or exist)
      }
      assert(e1.message === Some(allError(left1, didNotEqual(imaginaryFile.toPath, existFile.toPath) + ", and " + doesNotExist(imaginaryFile.toPath), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left2 = List(imaginaryFile.toPath)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (exist or equal (existFile.toPath))
      }
      assert(e2.message === Some(allError(left2, doesNotExist(imaginaryFile.toPath) + ", and " + didNotEqual(imaginaryFile.toPath, existFile.toPath), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left3 = List(imaginaryFile.toPath)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (existFile.toPath) or exist)
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(imaginaryFile.toPath, existFile.toPath) + ", and " + doesNotExist(imaginaryFile.toPath), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left4 = List(imaginaryFile.toPath)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (exist or be (existFile.toPath))
      }
      assert(e4.message === Some(allError(left4, doesNotExist(imaginaryFile.toPath) + ", and " + wasNotEqualTo(imaginaryFile.toPath, existFile.toPath), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should do nothing when it is used with not and the path does not exists") {
      all(List(imaginaryFile.toPath)) should (equal (imaginaryFile.toPath) or not (exist))
      all(List(imaginaryFile.toPath)) should (equal (existFile.toPath) or not (exist))
      all(List(existFile.toPath)) should (equal (existFile.toPath) or not (exist))

      all(List(imaginaryFile.toPath)) should (not (exist) or equal (imaginaryFile.toPath))
      all(List(existFile.toPath)) should (not (exist) or equal (existFile.toPath))
      all(List(imaginaryFile.toPath)) should (not (exist) or equal (existFile.toPath))

      all(List(imaginaryFile.toPath)) should (be (imaginaryFile.toPath) or not (exist))
      all(List(imaginaryFile.toPath)) should (be (existFile.toPath) or not (exist))
      all(List(existFile.toPath)) should (be (existFile.toPath) or not (exist))

      all(List(imaginaryFile.toPath)) should (not (exist) or be (imaginaryFile.toPath))
      all(List(existFile.toPath)) should (not (exist) or be (existFile.toPath))
      all(List(imaginaryFile.toPath)) should (not (exist) or be (existFile.toPath))
    }

    it("should throw TFE with correct stack depth and message when it is used with not and the path exists") {
      val left1 = List(existFile.toPath)
      val e1 = intercept[exceptions.TestFailedException] {
        all(left1) should (equal (imaginaryFile.toPath) or not (exist))
      }
      assert(e1.message === Some(allError(left1, didNotEqual(existFile.toPath, imaginaryFile.toPath) + ", and " + exists(existFile.toPath), thisLineNumber - 2)))
      assert(e1.failedCodeFileName === Some(fileName))
      assert(e1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left2 = List(existFile.toPath)
      val e2 = intercept[exceptions.TestFailedException] {
        all(left2) should (not (exist) or equal (imaginaryFile.toPath))
      }
      assert(e2.message === Some(allError(left2, exists(existFile.toPath) + ", and " + didNotEqual(existFile.toPath, imaginaryFile.toPath), thisLineNumber - 2)))
      assert(e2.failedCodeFileName === Some(fileName))
      assert(e2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left3 = List(existFile.toPath)
      val e3 = intercept[exceptions.TestFailedException] {
        all(left3) should (be (imaginaryFile.toPath) or not (exist))
      }
      assert(e3.message === Some(allError(left3, wasNotEqualTo(existFile.toPath, imaginaryFile.toPath) + ", and " + exists(existFile.toPath), thisLineNumber - 2)))
      assert(e3.failedCodeFileName === Some(fileName))
      assert(e3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val left4 = List(existFile.toPath)
      val e4 = intercept[exceptions.TestFailedException] {
        all(left4) should (not (exist) or be (imaginaryFile.toPath))
      }
      assert(e4.message === Some(allError(left4, exists(existFile.toPath) + ", and " + wasNotEqualTo(existFile.toPath, imaginaryFile.toPath), thisLineNumber - 2)))
      assert(e4.failedCodeFileName === Some(fileName))
      assert(e4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
}
