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

import SharedHelpers.thisLineNumber
import Matchers._
import exceptions.TestFailedException

class ShouldBeWritableStructuralSpec extends FunSpec {
  
  val fileName: String = "ShouldBeWritableStructuralSpec.scala"
    
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
      
      it("should do nothing for 'objTrue should be (writable)'") {
        objTrue should be (writable)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (writable)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be writable'") {
        objFalse should not be writable
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be writable'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be writable
        }
        assert(caught1.message === Some(wasWritable(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isWritable method") {
      
      class MyWritability(value: Boolean) {
        def isWritable: Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for 'objTrue should be (writable)'") {
        objTrue should be (writable)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (writable)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be writable'") {
        objFalse should not be writable
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be writable'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be writable
        }
        assert(caught1.message === Some(wasWritable(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isWritable val") {
      
      class MyWritability(value: Boolean) {
        val isWritable: Boolean = value
        override def toString = "writability"
      }
      val objTrue = new MyWritability(true)
      val objFalse = new MyWritability(false)
      
      it("should do nothing for 'objTrue should be (writable)'") {
        objTrue should be (writable)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (writable)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be writable'") {
        objFalse should not be writable
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be writable'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be writable
        }
        assert(caught1.message === Some(wasWritable(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
