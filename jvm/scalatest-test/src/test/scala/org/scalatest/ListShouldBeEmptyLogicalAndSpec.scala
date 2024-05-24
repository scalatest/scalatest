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
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ListShouldBeEmptyLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldBeEmptyLogicalAndSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages.wasNotEqualTo(prettifier, leftee, rightee)
  }
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
    
  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages.didNotEqual(prettifier, leftee, rightee)
  }
  
  def wasNotEmpty(left: Any): String = 
    FailureMessages.wasNotEmpty(prettifier, left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages.wasEmpty(prettifier, left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
    
  val nonEmptyList = List(1, 2, 3)
  val emptyList = List.empty[Int]
  
  describe("Emptiness matcher") {
    
    describe("when work with 'list should be (empty)'") {
      
      it("should do nothing when list is empty") {
        emptyList should (equal (emptyList) and be (empty))
        emptyList should (be (empty) and equal (emptyList))
        
        emptyList should (be (emptyList) and be (empty))
        emptyList should (be (empty) and be (emptyList))
      }
      
      it("should throw TestFailedException with correct stack depth when list is not empty") {
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should (equal (nonEmptyList) and be (empty))
        }
        assert(caught1.message === Some(equaled(nonEmptyList, nonEmptyList) + ", but " + wasNotEmpty(nonEmptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          nonEmptyList should (be (empty) and equal (nonEmptyList))
        }
        assert(caught2.message === Some(wasNotEmpty(nonEmptyList)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should (be (nonEmptyList) and be (empty))
        }
        assert(caught3.message === Some(wasEqualTo(nonEmptyList, nonEmptyList) + ", but " + wasNotEmpty(nonEmptyList)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          nonEmptyList should (be (empty) and be (nonEmptyList))
        }
        assert(caught4.message === Some(wasNotEmpty(nonEmptyList)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'list should not be empty'") {
      
      it("should do nothing when file is not empty") {
        nonEmptyList should (not equal emptyList and not be empty)
        nonEmptyList should (not be empty and not equal emptyList)
        
        nonEmptyList should (not be emptyList and not be empty)
        nonEmptyList should (not be empty and not be emptyList)
      }
      
      it("should throw TestFailedException with correct stack depth when list is not empty") {
        val caught1 = intercept[TestFailedException] {
          emptyList should (not equal nonEmptyList and not be empty)
        }
        assert(caught1.message === Some(didNotEqual(emptyList, nonEmptyList) + ", but " + wasEmpty(emptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyList should (not be empty and not equal nonEmptyList)
        }
        assert(caught2.message === Some(wasEmpty(emptyList)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          emptyList should (not be nonEmptyList and not be empty)
        }
        assert(caught3.message === Some(wasNotEqualTo(emptyList, nonEmptyList) + ", but " + wasEmpty(emptyList)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          emptyList should (not be empty and not be nonEmptyList)
        }
        assert(caught4.message === Some(wasEmpty(emptyList)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (empty)'") {
      
      it("should do nothing when all(xs) is empty") {
        all(List(emptyList)) should (be (emptyList) and be (empty))
        all(List(emptyList)) should (be (empty) and be (emptyList))
        
        all(List(emptyList)) should (equal (emptyList) and be (empty))
        all(List(emptyList)) should (be (empty) and equal (emptyList))
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not empty") {
        val left1 = List(nonEmptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (nonEmptyList) and be (empty))
        }
        assert(caught1.message === Some(allError(wasEqualTo(nonEmptyList, nonEmptyList) + ", but " + wasNotEmpty(nonEmptyList), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nonEmptyList)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (empty) and be (nonEmptyList))
        }
        assert(caught2.message === Some(allError(wasNotEmpty(nonEmptyList), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nonEmptyList)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (nonEmptyList) and be (empty))
        }
        assert(caught3.message === Some(allError(equaled(nonEmptyList, nonEmptyList) + ", but " + wasNotEmpty(nonEmptyList), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nonEmptyList)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (empty) and equal (nonEmptyList))
        }
        assert(caught4.message === Some(allError(wasNotEmpty(nonEmptyList), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be empty'") {
      it("should do nothing when all(xs) is not empty") {
        all(List(nonEmptyList)) should (not be empty and not be emptyList)
        all(List(nonEmptyList)) should (not be emptyList and not be empty)
        
        all(List(nonEmptyList)) should (not be empty and not equal emptyList)
        all(List(nonEmptyList)) should (not equal emptyList and not be empty)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is empty") {
        val left1 = List(emptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be nonEmptyList and not be empty)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(emptyList, nonEmptyList) + ", but " + wasEmpty(emptyList), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyList)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be empty and not be nonEmptyList)
        }
        assert(caught2.message === Some(allError(wasEmpty(emptyList), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(emptyList)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal nonEmptyList and not be empty)
        }
        assert(caught3.message === Some(allError(didNotEqual(emptyList, nonEmptyList) + ", but " + wasEmpty(emptyList), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(emptyList)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be empty and not equal nonEmptyList)
        }
        assert(caught4.message === Some(allError(wasEmpty(emptyList), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
