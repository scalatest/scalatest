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

class ShouldBeReadableLogicalOrImplicitSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  val fileName: String = "ShouldBeReadableLogicalOrImplicitSpec.scala"
  
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
  
  describe("Sorted matcher") {
    
    describe("when work with 'file should be (readable)'") {
      
      it("should do nothing when file is readable") {
        
        book should (be (readable) or be (book))
        stone should (be (readable) or be (stone))
        book should (be (readable) or be (stone))
        
        book should (be (book) or be (readable))
        book should (be (stone) or be (readable))
        stone should (be (stone) or be (readable))
        
        book should (be (readable) or equal (book))
        stone should (be (readable) or equal (stone))
        book should (be (readable) or equal (stone))
        
        book should (equal (book) or be (readable))
        book should (equal (stone) or be (readable))
        stone should (equal (stone) or be (readable))
      }
      
      it("should throw TestFailedException with correct stack depth when file is not readable") {
        val caught1 = intercept[TestFailedException] {
          stone should (be (readable) or be (book))
        }
        assert(caught1.message === Some(wasNotReadable(stone) + ", and " + wasNotEqualTo(stone, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          stone should (be (book) or be (readable))
        }
        assert(caught2.message === Some(wasNotEqualTo(stone, book) + ", and " + wasNotReadable(stone)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          stone should (be (readable) or equal (book))
        }
        assert(caught3.message === Some(wasNotReadable(stone) + ", and " + didNotEqual(stone, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          stone should (equal (book) or be (readable))
        }
        assert(caught4.message === Some(didNotEqual(stone, book) + ", and " + wasNotReadable(stone)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'file should not be readable'") {
      
      it("should do nothing when file is not readable") {
        stone should (not be readable or not be book)
        book should (not be readable or not be stone)
        stone should (not be readable or not be stone)
        
        stone should (not be book or not be readable)
        stone should (not be stone or not be readable)
        book should (not be stone or not be readable)
        
        stone should (not be readable or not equal book)
        book should (not be readable or not equal stone)
        stone should (not be readable or not equal stone)
        
        stone should (not equal book or not be readable)
        stone should (not equal stone or not be readable)
        book should (not equal stone or not be readable)
      }
      
      it("should throw TestFailedException with correct stack depth when file is readable") {
        val caught1 = intercept[TestFailedException] {
          book should (not be readable or not be book)
        }
        assert(caught1.message === Some(wasReadable(book) + ", and " + wasEqualTo(book, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          book should (not be book or not be readable)
        }
        assert(caught2.message === Some(wasEqualTo(book, book) + ", and " + wasReadable(book)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          book should (not be readable or not equal book)
        }
        assert(caught3.message === Some(wasReadable(book) + ", and " + equaled(book, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          book should (not equal book or not be readable)
        }
        assert(caught4.message === Some(equaled(book, book) + ", and " + wasReadable(book)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should be (readable)'") {
      
      it("should do nothing when all(xs) is readable") {
        all(List(book)) should (be (readable) or be (book))
        all(List(stone)) should (be (readable) or be (stone))
        all(List(book)) should (be (readable) or be (stone))
        
        all(List(book)) should (be (book) or be (readable))
        all(List(book)) should (be (stone) or be (readable))
        all(List(stone)) should (be (stone) or be (readable))
        
        all(List(book)) should (be (readable) or equal (book))
        all(List(stone)) should (be (readable) or equal (stone))
        all(List(book)) should (be (readable) or equal (stone))
        
        all(List(book)) should (equal (book) or be (readable))
        all(List(book)) should (equal (stone) or be (readable))
        all(List(stone)) should (equal (stone) or be (readable))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (book) or be (readable))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(stone, book) + ", and " + wasNotReadable(stone), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(stone)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (readable) or be (book))
        }
        assert(caught2.message === Some(allError(wasNotReadable(stone) + ", and " + wasNotEqualTo(stone, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(stone)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (book) or be (readable))
        }
        assert(caught3.message === Some(allError(didNotEqual(stone, book) + ", and " + wasNotReadable(stone), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(stone)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (readable) or equal (book))
        }
        assert(caught4.message === Some(allError(wasNotReadable(stone) + ", and " + didNotEqual(stone, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("when work with 'all(xs) should not be sorted'") {
      it("should do nothing when xs is not sorted") {
        all(List(stone)) should (not be readable or not be book)
        all(List(book)) should (not be readable or not be stone)
        all(List(stone)) should (not be readable or not be stone)
        
        all(List(stone)) should (not be book or not be readable)
        all(List(stone)) should (not be stone or not be readable)
        all(List(book)) should (not be stone or not be readable)
        
        all(List(stone)) should (not be readable or not equal book)
        all(List(book)) should (not be readable or not equal stone)
        all(List(stone)) should (not be readable or not equal stone)
        
        all(List(stone)) should (not equal book or not be readable)
        all(List(stone)) should (not equal stone or not be readable)
        all(List(book)) should (not equal stone or not be readable)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be book or not be readable)
        }
        assert(caught1.message === Some(allError(wasEqualTo(book, book) + ", and " + wasReadable(book), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(book)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be readable or not be book)
        }
        assert(caught2.message === Some(allError(wasReadable(book) + ", and " + wasEqualTo(book, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(book)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal book or not be readable)
        }
        assert(caught3.message === Some(allError(equaled(book, book) + ", and " + wasReadable(book), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(book)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be readable or not equal book)
        }
        assert(caught4.message === Some(allError(wasReadable(book) + ", and " + equaled(book, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
