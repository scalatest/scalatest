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

class ShouldBeReadableStructuralLogicalOrSpec extends FunSpec with Matchers {
  
  val fileName: String = "ShouldBeReadableStructuralLogicalOrSpec.scala"
  
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
  
  describe("readable matcher") {
    describe("when work with arbitrary object with isReadable() method") {
      
      class MyReadability(value: Boolean) {
        def isReadable(): Boolean = value
        override def toString = "readability"
      }
      val objTrue = new MyReadability(true)
      val objFalse = new MyReadability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (readable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (readable))
        objTrue should (be (readable) or be (objTrue))
        objTrue should (be (objTrue) or be (readable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (readable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (readable))
        objFalse should (be (readable) or be (objFalse))
        objTrue should (be (objFalse) or be (readable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (readable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (readable))
        objTrue should (be (readable) or be (objFalse))
        objFalse should (be (objFalse) or be (readable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
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
        objTrue should (be (readable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (readable))
        objTrue should (be (readable) or be (objTrue))
        objTrue should (be (objTrue) or be (readable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (readable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (readable))
        objFalse should (be (readable) or be (objFalse))
        objTrue should (be (objFalse) or be (readable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (readable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (readable))
        objTrue should (be (readable) or be (objFalse))
        objFalse should (be (objFalse) or be (readable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
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
        objTrue should (be (readable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (readable))
        objTrue should (be (readable) or be (objTrue))
        objTrue should (be (objTrue) or be (readable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (readable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (readable))
        objFalse should (be (readable) or be (objFalse))
        objTrue should (be (objFalse) or be (readable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (readable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (readable))
        objTrue should (be (readable) or be (objFalse))
        objFalse should (be (objFalse) or be (readable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (readable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotReadable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (readable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (readable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotReadable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (readable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotReadable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}