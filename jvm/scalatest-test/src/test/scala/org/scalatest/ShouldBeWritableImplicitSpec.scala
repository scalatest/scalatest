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

import SharedHelpers.{createTempDirectory, thisLineNumber}
import enablers.Writability
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeWritableImplicitSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  trait Thing {
    def canRead: Boolean
  }
  
  val book = new Thing {
    val canRead = true
  }
  
  val stone = new Thing {
    val canRead = false
  }
  
  implicit def writabilityOfThing[T <: Thing]: Writability[T] =
    new Writability[T] {
      def isWritable(thing: T): Boolean = thing.canRead
    }
  
  val fileName: String = "ShouldBeWritableImplicitSpec.scala"
    
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(prettifier, left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(prettifier, left)
  
  it("book should be writable, stone should not be writable") {
    assert(book.canRead === true)
    assert(stone.canRead === false)
  }
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
  
  describe("Writable matcher") {
    
    describe("when work with 'file should be (writable)'") {
      
      it("should do nothing when file is writable") {
        book should be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          stone should be (writable)
        }
        assert(caught1.message === Some(wasNotWritable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'file should not be writable'") {
      
      it("should do nothing when file is not writable") {
        stone should not be writable
      }
      
      it("should throw TestFailedException with correct stack depth when file is writable") {
        val caught1 = intercept[TestFailedException] {
          book should not be writable
        }
        assert(caught1.message === Some(wasWritable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file shouldBe writable'") {
      
      it("should do nothing when file is writable") {
        book shouldBe writable
      }
      
      it("should throw TestFailedException with correct stack depth when file is not writable") {
        val caught1 = intercept[TestFailedException] {
          stone shouldBe writable
        }
        assert(caught1.message === Some(wasNotWritable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'file shouldNot be (writable)'") {
      
      it("should do nothing when file is not writable") {
        stone shouldNot be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is writable") {
        val caught1 = intercept[TestFailedException] {
          book shouldNot be (writable)
        }
        assert(caught1.message === Some(wasWritable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should be (writable)'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(book)) should be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not writable") {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) should not be writable'") {
      
      it("should do nothing when all(xs) is not writable") {
        all(List(stone)) should not be writable
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is writable") {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be writable
        }
        assert(caught1.message === Some(allError(left1, wasWritable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when work with 'all(xs) shouldBe writable'") {
      
      it("should do nothing when all(xs) is writable") {
        all(List(book)) shouldBe writable
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not writable") {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe writable
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) shouldNot be (writable)'") {
      
      it("should do nothing when all(xs) is not writable") {
        all(List(stone)) shouldNot be (writable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is writable") {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (writable)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
