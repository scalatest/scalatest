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

class ShouldBeDefinedStructuralLogicalOrSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeDefinedStructuralLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(prettifier, left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(prettifier, left, right)
  
  def wasNotDefined(left: Any): String = 
    FailureMessages.wasNotDefined(prettifier, left)
    
  def wasDefined(left: Any): String = 
    FailureMessages.wasDefined(prettifier, left)
  
  describe("Defined matcher") {
    describe("when work with arbitrary object with isDefined() method") {
      
      class MyDefinition(value: Boolean) {
        def isDefined(): Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (defined) or equal (objTrue))
        objTrue should (equal (objTrue) or be (defined))
        objTrue should (be (defined) or be (objTrue))
        objTrue should (be (objTrue) or be (defined))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (defined) or equal (objFalse))
        objTrue should (equal (objFalse) or be (defined))
        objFalse should (be (defined) or be (objFalse))
        objTrue should (be (objFalse) or be (defined))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (defined) or equal (objFalse))
        objFalse should (equal (objFalse) or be (defined))
        objTrue should (be (defined) or be (objFalse))
        objFalse should (be (objFalse) or be (defined))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (defined) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotDefined(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (defined))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (defined) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotDefined(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (defined))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isDefined method") {
      
      class MyDefinition(value: Boolean) {
        def isDefined: Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (defined) or equal (objTrue))
        objTrue should (equal (objTrue) or be (defined))
        objTrue should (be (defined) or be (objTrue))
        objTrue should (be (objTrue) or be (defined))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (defined) or equal (objFalse))
        objTrue should (equal (objFalse) or be (defined))
        objFalse should (be (defined) or be (objFalse))
        objTrue should (be (objFalse) or be (defined))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (defined) or equal (objFalse))
        objFalse should (equal (objFalse) or be (defined))
        objTrue should (be (defined) or be (objFalse))
        objFalse should (be (objFalse) or be (defined))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (defined) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotDefined(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (defined))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (defined) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotDefined(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (defined))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with arbitrary object with isDefined val") {
      
      class MyDefinition(value: Boolean) {
        val isDefined: Boolean = value
        override def toString = "definition"
      }
      val objTrue = new MyDefinition(true)
      val objFalse = new MyDefinition(false)
      
      it("should do nothing for when both passed") {
        objTrue should (be (defined) or equal (objTrue))
        objTrue should (equal (objTrue) or be (defined))
        objTrue should (be (defined) or be (objTrue))
        objTrue should (be (objTrue) or be (defined))
      }
      
      it("should do nothing when first check failed") {
        objFalse should (be (defined) or equal (objFalse))
        objTrue should (equal (objFalse) or be (defined))
        objFalse should (be (defined) or be (objFalse))
        objTrue should (be (objFalse) or be (defined))
      }
      
      it("should do nothing when second check failed") {
        objTrue should (be (defined) or equal (objFalse))
        objFalse should (equal (objFalse) or be (defined))
        objTrue should (be (defined) or be (objFalse))
        objFalse should (be (objFalse) or be (defined))
      }
      
      it("should throw correct TFE when both check failed") {
        val caught1 = intercept[TestFailedException] {
          objFalse should (be (defined) or equal (objTrue))
        }
        assert(caught1.message === Some(wasNotDefined(objFalse) + ", and " + didNotEqual(objFalse, objTrue)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          objFalse should (equal (objTrue) or be (defined))
        }
        assert(caught2.message === Some(didNotEqual(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          objFalse should (be (defined) or be (objTrue))
        }
        assert(caught3.message === Some(wasNotDefined(objFalse) + ", and " + wasNotEqualTo(objFalse, objTrue)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          objFalse should (be (objTrue) or be (defined))
        }
        assert(caught4.message === Some(wasNotEqualTo(objFalse, objTrue) + ", and " + wasNotDefined(objFalse)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
