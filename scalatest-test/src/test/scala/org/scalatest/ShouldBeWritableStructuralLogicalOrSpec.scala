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

class ShouldBeWritableStructuralLogicalOrSpec extends FunSpec {
  
  val fileName: String = "ShouldBeWritableStructuralLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotWritable(left: Any): String = 
    FailureMessages("wasNotWritable", left)
    
  def wasWritable(left: Any): String = 
    FailureMessages("wasWritable", left)
    
  describe("writable matcher") {
    
    describe("when work with arbitrary object with isWritable() method") {
      
      class MyWritability(value: Boolean) {
        def isWritable(): Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (writable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (writable))
        objTrue should (be (writable) or be (objTrue))
        objTrue should (be (objTrue) or be (writable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (writable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (writable))
        objFalse should (be (writable) or be (objFalse))
        objTrue should (be (objFalse) or be (writable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (writable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (writable))
        objTrue should (be (writable) or be (objFalse))
        objFalse should (be (objFalse) or be (writable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
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
        objTrue should (be (writable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (writable))
        objTrue should (be (writable) or be (objTrue))
        objTrue should (be (objTrue) or be (writable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (writable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (writable))
        objFalse should (be (writable) or be (objFalse))
        objTrue should (be (objFalse) or be (writable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (writable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (writable))
        objTrue should (be (writable) or be (objFalse))
        objFalse should (be (objFalse) or be (writable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
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
        objTrue should (be (writable) or equal (objTrue))
        objTrue should (equal (objTrue) or be (writable))
        objTrue should (be (writable) or be (objTrue))
        objTrue should (be (objTrue) or be (writable))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (writable) or equal (objFalse))
        objTrue should (equal (objFalse) or be (writable))
        objFalse should (be (writable) or be (objFalse))
        objTrue should (be (objFalse) or be (writable))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (writable) or equal (objFalse))
        objFalse should (equal (objFalse) or be (writable))
        objTrue should (be (writable) or be (objFalse))
        objFalse should (be (objFalse) or be (writable))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (writable) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotWritable(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (writable))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (writable) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotWritable(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (writable))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotWritable(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
