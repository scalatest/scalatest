/*
 * Copyright 2001-2013 Artima, Inc.
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
import Matchers._
import exceptions.TestFailedException

class ShouldBeReadableStructuralLogicalAndSpec extends FunSpec {
  
  val fileName: String = "ShouldBeReadableStructuralLogicalAndSpec.scala"
  
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
  
  describe("readable matcher") {
    describe("when work with arbitrary object with isReadable() method") {
      
      class MyReadability(value: Boolean) {
        def isReadable(): Boolean = value
        override def toString = "readability"
      }
      val objTrue = new MyReadability(true)
      val objFalse = new MyReadability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (readable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (readable))
        objTrue should (be (readable) and be (objTrue))
        objTrue should (be (objTrue) and be (readable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasReadable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasReadable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotReadable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isReadable method") {
      
      class MyReadability(value: Boolean) {
        def isReadable: Boolean = value
        override def toString = "readability"
      }
      val objTrue = new MyReadability(true)
      val objFalse = new MyReadability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (readable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (readable))
        objTrue should (be (readable) and be (objTrue))
        objTrue should (be (objTrue) and be (readable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasReadable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasReadable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotReadable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isReadable val") {
      
      class MyReadability(value: Boolean) {
        val isReadable: Boolean = value
        override def toString = "readability"
      }
      val objTrue = new MyReadability(true)
      val objFalse = new MyReadability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (readable) and equal (objTrue))
        objTrue should (equal (objTrue) and be (readable))
        objTrue should (be (readable) and be (objTrue))
        objTrue should (be (objTrue) and be (readable))
      }
      
      it("should throw correct TFE when first check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objTrue should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objTrue, objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objTrue should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objTrue, objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when second check failed") {
        val caught1 = intercept[TestFailedException] {
          objTrue should (be (readable) and equal (objFalse))
        }
        assert(caught1.message === Some(wasReadable(objTrue) + ", but " + didNotEqual(objTrue, objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objFalse) and be (readable))
        }
        assert(caught2.message === Some(equaled(objFalse, objFalse) + ", but " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objTrue should (be (readable) and be (objFalse))
        }
        assert(caught3.message === Some(wasReadable(objTrue) + ", but " + wasNotEqualTo(objTrue, objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objFalse) and be (readable))
        }
        assert(caught4.message === Some(wasEqualTo(objFalse, objFalse) + ", but " +  wasNotReadable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) and equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) and be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) and be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) and be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
