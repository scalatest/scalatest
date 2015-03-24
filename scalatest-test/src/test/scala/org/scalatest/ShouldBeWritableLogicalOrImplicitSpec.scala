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
import enablers.Writability
import Matchers._
import exceptions.TestFailedException

class ShouldBeWritableLogicalOrImplicitSpec extends Spec {
  
  val fileName: String = "ShouldBeWritableLogicalOrImplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(left, right)
  
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
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
  
  implicit def writabilityOfThing[T <: Thing]: Writability[T] =
    new Writability[T] {
      def isWritable(thing: T): Boolean = thing.canRead
    }
  
  object `Sorted matcher` {
    
    object `when work with 'file should be (writable)'` {
      
      def `should do nothing when file is writable` {
        
        book should (be (writable) or be_== (book))
        stone should (be (writable) or be_== (stone))
        book should (be (writable) or be_== (stone))
        
        book should (be_== (book) or be (writable))
        book should (be_== (stone) or be (writable))
        stone should (be_== (stone) or be (writable))
        
        book should (be (writable) or equal (book))
        stone should (be (writable) or equal (stone))
        book should (be (writable) or equal (stone))
        
        book should (equal (book) or be (writable))
        book should (equal (stone) or be (writable))
        stone should (equal (stone) or be (writable))
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          stone should (be (writable) or be_== (book))
        }
        assert(caught1.message === Some(wasNotWritable(stone) + ", and " + wasNotEqualTo(stone, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          stone should (be_== (book) or be (writable))
        }
        assert(caught2.message === Some(wasNotEqualTo(stone, book) + ", and " + wasNotWritable(stone)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          stone should (be (writable) or equal (book))
        }
        assert(caught3.message === Some(wasNotWritable(stone) + ", and " + didNotEqual(stone, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          stone should (equal (book) or be (writable))
        }
        assert(caught4.message === Some(didNotEqual(stone, book) + ", and " + wasNotWritable(stone)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be writable'` {
      
      def `should do nothing when file is not writable` {
        stone should (not be writable or not be_== book)
        book should (not be writable or not be_== stone)
        stone should (not be writable or not be_== stone)
        
        stone should (not be_== book or not be writable)
        stone should (not be_== stone or not be writable)
        book should (not be_== stone or not be writable)
        
        stone should (not be writable or not equal book)
        book should (not be writable or not equal stone)
        stone should (not be writable or not equal stone)
        
        stone should (not equal book or not be writable)
        stone should (not equal stone or not be writable)
        book should (not equal stone or not be writable)
      }
      
      def `should throw TestFailedException with correct stack depth when file is writable` {
        val caught1 = intercept[TestFailedException] {
          book should (not be writable or not be_== book)
        }
        assert(caught1.message === Some(wasWritable(book) + ", and " + wasEqualTo(book, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          book should (not be_== book or not be writable)
        }
        assert(caught2.message === Some(wasEqualTo(book, book) + ", and " + wasWritable(book)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          book should (not be writable or not equal book)
        }
        assert(caught3.message === Some(wasWritable(book) + ", and " + equaled(book, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          book should (not equal book or not be writable)
        }
        assert(caught4.message === Some(equaled(book, book) + ", and " + wasWritable(book)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (writable)'` {
      
      def `should do nothing when all(xs) is writable` {
        all(List(book)) should (be (writable) or be_== (book))
        all(List(stone)) should (be (writable) or be_== (stone))
        all(List(book)) should (be (writable) or be_== (stone))
        
        all(List(book)) should (be_== (book) or be (writable))
        all(List(book)) should (be_== (stone) or be (writable))
        all(List(stone)) should (be_== (stone) or be (writable))
        
        all(List(book)) should (be (writable) or equal (book))
        all(List(stone)) should (be (writable) or equal (stone))
        all(List(book)) should (be (writable) or equal (stone))
        
        all(List(book)) should (equal (book) or be (writable))
        all(List(book)) should (equal (stone) or be (writable))
        all(List(stone)) should (equal (stone) or be (writable))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be_== (book) or be (writable))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(stone, book) + ", and " + wasNotWritable(stone), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(stone)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (writable) or be_== (book))
        }
        assert(caught2.message === Some(allError(wasNotWritable(stone) + ", and " + wasNotEqualTo(stone, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(stone)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (book) or be (writable))
        }
        assert(caught3.message === Some(allError(didNotEqual(stone, book) + ", and " + wasNotWritable(stone), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(stone)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (writable) or equal (book))
        }
        assert(caught4.message === Some(allError(wasNotWritable(stone) + ", and " + didNotEqual(stone, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(stone)) should (not be writable or not be_== book)
        all(List(book)) should (not be writable or not be_== stone)
        all(List(stone)) should (not be writable or not be_== stone)
        
        all(List(stone)) should (not be_== book or not be writable)
        all(List(stone)) should (not be_== stone or not be writable)
        all(List(book)) should (not be_== stone or not be writable)
        
        all(List(stone)) should (not be writable or not equal book)
        all(List(book)) should (not be writable or not equal stone)
        all(List(stone)) should (not be writable or not equal stone)
        
        all(List(stone)) should (not equal book or not be writable)
        all(List(stone)) should (not equal stone or not be writable)
        all(List(book)) should (not equal stone or not be writable)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be_== book or not be writable)
        }
        assert(caught1.message === Some(allError(wasEqualTo(book, book) + ", and " + wasWritable(book), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(book)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be writable or not be_== book)
        }
        assert(caught2.message === Some(allError(wasWritable(book) + ", and " + wasEqualTo(book, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(book)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal book or not be writable)
        }
        assert(caught3.message === Some(allError(equaled(book, book) + ", and " + wasWritable(book), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(book)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be writable or not equal book)
        }
        assert(caught4.message === Some(allError(wasWritable(book) + ", and " + equaled(book, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
