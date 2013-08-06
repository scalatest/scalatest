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

class ShouldBeDefinedStructuralSpec extends FunSpec with Matchers {
  
  val fileName: String = "ShouldBeDefinedStructuralSpec.scala"
    
  def wasNotDefined(left: Any): String = 
    FailureMessages("wasNotDefined", left)
    
  def wasDefined(left: Any): String = 
    FailureMessages("wasDefined", left)
  
  describe("defined matcher") {
    
    describe("when work with arbitrary object with isDefined() method") {
      
      class MyDefinition(value: Boolean) {
        def isDefined(): Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for 'objTrue should be (defined)'") {
        objTrue should be (defined)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (defined)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (defined)
        }
        assert(caught1.message === Some(wasNotDefined(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be defined'") {
        objFalse should not be defined
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be defined'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be defined
        }
        assert(caught1.message === Some(wasDefined(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isDefined method") {
      
      class MyDefinition(value: Boolean) {
        def isDefined: Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for 'objTrue should be (defined)'") {
        objTrue should be (defined)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (defined)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (defined)
        }
        assert(caught1.message === Some(wasNotDefined(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be defined'") {
        objFalse should not be defined
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be defined'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be defined
        }
        assert(caught1.message === Some(wasDefined(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isDefined val") {
      
      class MyDefinition(value: Boolean) {
        val isDefined: Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for 'objTrue should be (defined)'") {
        objTrue should be (defined)
      }
      
      it("should throw TFE with correct stack depth for 'objFalse should be (defined)'") {
        val caught1 = intercept[TestFailedException] {
          objFalse should be (defined)
        }
        assert(caught1.message === Some(wasNotDefined(objFalse)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing if for 'objFalse should not be defined'") {
        objFalse should not be defined
      }
      
      it("should throw TFE with correct stack depth for 'objTrue should not be defined'") {
        val caught1 = intercept[TestFailedException] {
          objTrue should not be defined
        }
        assert(caught1.message === Some(wasDefined(objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
  }
  
}