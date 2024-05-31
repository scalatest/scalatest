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

import SharedHelpers.{thisLineNumber, createTempDirectory}
import enablers.Readability
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeReadableLogicalAndImplicitSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeReadableLogicalAndImplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(prettifier, left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(prettifier, left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(prettifier, left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(prettifier, left, right)
  
  def wasNotReadable(left: Any): String = 
    FailureMessages.wasNotReadable(prettifier, left)
    
  def wasReadable(left: Any): String = 
    FailureMessages.wasReadable(prettifier, left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsIterableMessageWithStackDepth(prettifier, 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(prettifier, messageWithIndex, left)
  }
    
  trait Thing {
    def canRead: Boolean
  }
  
  val book = new Thing {
    val canRead = true
  }
  
  val stone = new Thing {
    val canRead = false
  }
  
  implicit def readabilityOfThing[T <: Thing]: Readability[T] =
    new Readability[T] {
      def isReadable(thing: T): Boolean = thing.canRead
    }
  
  describe("Readability matcher") {
    
    describe("when work with 'file should be (readable)'") {
      
      it("should do nothing when file is readable") {
        book should (equal (book) and be (readable))
        book should (be (readable) and equal (book))
        
        book should (be (book) and be (readable))
        book should (be (readable) and be (book))
      }
      
      it("should throw TestFailedException with correct stack depth when file is not readable") {
        val caught1 = intercept[TestFailedException] {
          stone should (equal (stone) and be (readable))
        }
        assert(caught1.message === Some(equaled(stone, stone) + ", but " + wasNotReadable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          stone should (be (readable) and equal (stone))
        }
        assert(caught2.message === Some(wasNotReadable(stone)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          stone should (be (stone) and be (readable))
        }
        assert(caught3.message === Some(wasEqualTo(stone, stone) + ", but " + wasNotReadable(stone)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          stone should (be (readable) and be (stone))
        }
        assert(caught4.message === Some(wasNotReadable(stone)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file should not be sorted'") {
      
      it("should do nothing when file is not readable") {
        stone should (not equal book and not be readable)
        stone should (not be readable and not equal book)
        
        stone should (not be book and not be readable)
        stone should (not be readable and not be book)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val caught1 = intercept[TestFailedException] {
          book should (not equal stone and not be readable)
        }
        assert(caught1.message === Some(didNotEqual(book, stone) + ", but " + wasReadable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          book should (not be readable and not equal stone)
        }
        assert(caught2.message === Some(wasReadable(book)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          book should (not be stone and not be readable)
        }
        assert(caught3.message === Some(wasNotEqualTo(book, stone) + ", but " + wasReadable(book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          book should (not be readable and not be stone)
        }
        assert(caught4.message === Some(wasReadable(book)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (readable)'") {
      
      it("should do nothing when all(xs) is readable") {
        all(List(book)) should (be (book) and be (readable))
        all(List(book)) should (be (readable) and be (book))
        
        all(List(book)) should (equal (book) and be (readable))
        all(List(book)) should (be (readable) and equal (book))
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is not readable") {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (stone) and be (readable))
        }
        assert(caught1.message === Some(allError(wasEqualTo(stone, stone) + ", but " + wasNotReadable(stone), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(stone)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (readable) and be (stone))
        }
        assert(caught2.message === Some(allError(wasNotReadable(stone), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(stone)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (stone) and be (readable))
        }
        assert(caught3.message === Some(allError(equaled(stone, stone) + ", but " + wasNotReadable(stone), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(stone)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (readable) and equal (stone))
        }
        assert(caught4.message === Some(allError(wasNotReadable(stone), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be readable'") {
      it("should do nothing when all(xs) is not readable") {
        all(List(stone)) should (not be readable and not be book)
        all(List(stone)) should (not be book and not be readable)
        
        all(List(stone)) should (not be readable and not equal book)
        all(List(stone)) should (not equal book and not be readable)
      }
      
      it("should throw TestFailedException with correct stack depth when all(xs) is readable") {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be stone and not be readable)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(book, stone) + ", but " + wasReadable(book), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(book)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be readable and not be stone)
        }
        assert(caught2.message === Some(allError(wasReadable(book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(book)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal stone and not be readable)
        }
        assert(caught3.message === Some(allError(didNotEqual(book, stone) + ", but " + wasReadable(book), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(book)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be readable and not equal stone)
        }
        assert(caught4.message === Some(allError(wasReadable(book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
