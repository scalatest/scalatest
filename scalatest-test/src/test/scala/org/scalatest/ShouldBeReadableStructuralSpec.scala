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

class ShouldBeReadableStructuralSpec extends FunSpec {
  
  val fileName: String = "ShouldBeReadableStructuralSpec.scala"
    
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
      
      it("should do nothing for 'objTrue should be (readable)'") {
        objTrue should be (readable)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (readable)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (readable)
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be readable'") {
        objFalse should not be readable
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be readable'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be readable
        }
        assert(caught1.message === Some(wasReadable(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isReadable method") {
      
      class MyReadability(value: Boolean) {
        def isReadable: Boolean = value
        override def toString = "readability"
      }
      val objTrue = new MyReadability(true)
      val objFalse = new MyReadability(false)
      
      it("should do nothing for 'objTrue should be (readable)'") {
        objTrue should be (readable)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (readable)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (readable)
        }
        assert(caught1.message === Some(wasNotReadable(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be readable'") {
        objFalse should not be readable
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be readable'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be readable
        }
        assert(caught1.message === Some(wasReadable(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
  }
}
