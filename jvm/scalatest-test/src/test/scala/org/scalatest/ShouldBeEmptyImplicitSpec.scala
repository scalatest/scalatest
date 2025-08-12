/*
 * Copyright 2001-2025 Artima, Inc.
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
import enablers.Emptiness
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeEmptyImplicitSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  trait Thing {
    def isEmpty: Boolean
  }
  
  val nonEmptyThing = new Thing {
    val isEmpty = false
  }
  val emptyThing = new Thing {
    val isEmpty = true
  }
  
  implicit def emptinessOfThing: Emptiness[Thing] =
    new Emptiness[Thing] {
      def isEmpty(thing: Thing): Boolean = thing.isEmpty
    }
  
  val fileName: String = "ShouldBeEmptyImplicitSpec.scala"
    
  def wasNotEmpty(left: Any): String = 
    FailureMessages.wasNotEmpty(prettifier, left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages.wasEmpty(prettifier, left)
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
  
  describe("Empty matcher") {
    
    describe("when work with 'list should be (empty)'") {
      
      it("should do nothing when list is empty") {
        emptyThing should be (empty)
      }
      
      it("should throw TestFailedException with correct stack depth when list is not empty") {
        val caught1 = intercept[TestFailedException] {
          nonEmptyThing should be (empty)
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'list should not be empty'") {
      
      it("should do nothing when list is not empty") {
        nonEmptyThing should not be empty
      }
      
      it("should throw TestFailedException with correct stack depth when list is empty") {
        val caught1 = intercept[TestFailedException] {
          emptyThing should not be empty
        }
        assert(caught1.message === Some(wasEmpty(emptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'list shouldBe empty'") {
      
      it("should do nothing when list is empty") {
        emptyThing shouldBe empty
      }
      
      it("should throw TestFailedException with correct stack depth when list is not empty") {
        val caught1 = intercept[TestFailedException] {
          nonEmptyThing shouldBe empty
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'list shouldNot be (empty)'") {
      
      it("should do nothing when list is not empty") {
        nonEmptyThing shouldNot be (empty)
      }
      
      it("should throw TestFailedException with correct stack depth when list is empty") {
        val caught1 = intercept[TestFailedException] {
          emptyThing shouldNot be (empty)
        }
        assert(caught1.message === Some(wasEmpty(emptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should be (empty)'") {
      
      it("should do nothing when all(xs) is empty") {
        all(List(emptyThing)) should be (empty)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not empty") {
        val left1 = List(nonEmptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (empty)
        }
        assert(caught1.message === Some(allError(left1, wasNotEmpty(nonEmptyThing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should not be empty'") {
      
      it("should do nothing when all(xs) is not empty") {
        all(List(nonEmptyThing)) should not be empty
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is empty") {
        val left1 = List(emptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be empty
        }
        assert(caught1.message === Some(allError(left1, wasEmpty(emptyThing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) shouldBe empty'") {
      
      it("should do nothing when all(xs) is empty") {
        all(List(emptyThing)) shouldBe empty
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not empty") {
        val left1 = List(nonEmptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe empty
        }
        assert(caught1.message === Some(allError(left1, wasNotEmpty(nonEmptyThing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) shouldNot be (empty)'") {
      
      it("should do nothing when all(xs) is not empty") {
        all(List(nonEmptyThing)) shouldNot be (empty)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is empty") {
        val left1 = List(emptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (empty)
        }
        assert(caught1.message === Some(allError(left1, wasEmpty(emptyThing), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
