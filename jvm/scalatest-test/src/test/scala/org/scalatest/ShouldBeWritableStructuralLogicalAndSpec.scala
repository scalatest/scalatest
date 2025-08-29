/*
 * Copyright 2001-2025 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this opt except in compliance with the License.
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

import SharedHelpers.thisLineNumber
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeWritableStructuralLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeWritableStructuralLogicalAndSpec.scala"
  
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

  describe("writable matcher") {
    
    describe("when work with arbitrary object with isWritable() method") {
      
      class MyWritability(value: Boolean) {
        def isWritable(): Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (writable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (writable))
        objTrue should (be (writable) and be (objTrue))
        objTrue should (be (objTrue) and be (writable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasWritable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasWritable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotWritable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isWritable method") {
      
      class MyWritability(value: Boolean) {
        def isWritable: Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (writable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (writable))
        objTrue should (be (writable) and be (objTrue))
        objTrue should (be (objTrue) and be (writable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasWritable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasWritable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotWritable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isWritable val") {
      
      class MyWritability(value: Boolean) {
        val isWritable: Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (writable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (writable))
        objTrue should (be (writable) and be (objTrue))
        objTrue should (be (objTrue) and be (writable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (writable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasWritable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (writable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (writable) and be (objFalse))
        }
        assert(caught3.message === Some(wasWritable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (writable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotWritable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
