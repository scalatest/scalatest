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
import Matchers._
import exceptions.TestFailedException

class ShouldBeReadableLogicalOrExplicitSpec extends Spec {
  
  val fileName: String = "ShouldBeReadableLogicalOrExplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotReadable(left: Any): String = 
    FailureMessages("wasNotReadable", left)
    
  def wasReadable(left: Any): String = 
    FailureMessages("wasReadable", left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
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
  
  val readability = 
    new Readability[Thing] {
      def isReadable(thing: Thing): Boolean = thing.canRead
    }
  
  object `Sorted matcher` {
    
    object `when work with 'file should be (readable)'` {
      
      def `should do nothing when file is readable` {
        
        (book should (be (readable) or be_== (book))) (readability)
        (stone should (be (readable) or be_== (stone))) (readability)
        (book should (be (readable) or be_== (stone))) (readability)
        
        (book should (be_== (book) or be (readable))) (readability)
        (book should (be_== (stone) or be (readable))) (readability)
        (stone should (be_== (stone) or be (readable))) (readability)
        
        (book should (be (readable) or equal (book))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (stone should (be (readable) or equal (stone))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (book should (be (readable) or equal (stone))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        
        (book should (equal (book) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (book should (equal (stone) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (stone should (equal (stone) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not readable` {
        val caught1 = intercept[TestFailedException] {
          (stone should (be (readable) or be_== (book))) (readability)
        }
        assert(caught1.message === Some(wasNotReadable(stone) + ", and " + wasNotEqualTo(stone, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (stone should (be_== (book) or be (readable))) (readability)
        }
        assert(caught2.message === Some(wasNotEqualTo(stone, book) + ", and " + wasNotReadable(stone)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (stone should (be (readable) or equal (book))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        }
        assert(caught3.message === Some(wasNotReadable(stone) + ", and " + didNotEqual(stone, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (stone should (equal (book) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        }
        assert(caught4.message === Some(didNotEqual(stone, book) + ", and " + wasNotReadable(stone)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be readable'` {
      
      def `should do nothing when file is not readable` {
        (stone should (not be readable or not be_== book)) (readability)
        (book should (not be readable or not be_== stone)) (readability)
        (stone should (not be readable or not be_== stone)) (readability)
        
        (stone should (not be_== book or not be readable)) (readability)
        (stone should (not be_== stone or not be readable)) (readability)
        (book should (not be_== stone or not be readable)) (readability)
        
        (stone should (not be readable or not equal book)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (book should (not be readable or not equal stone)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (stone should (not be readable or not equal stone)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        
        (stone should (not equal book or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (stone should (not equal stone or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (book should (not equal stone or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is readable` {
        val caught1 = intercept[TestFailedException] {
          (book should (not be readable or not be_== book)) (readability)
        }
        assert(caught1.message === Some(wasReadable(book) + ", and " + wasEqualTo(book, book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (book should (not be_== book or not be readable)) (readability)
        }
        assert(caught2.message === Some(wasEqualTo(book, book) + ", and " + wasReadable(book)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (book should (not be readable or not equal book)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        }
        assert(caught3.message === Some(wasReadable(book) + ", and " + equaled(book, book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (book should (not equal book or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        }
        assert(caught4.message === Some(equaled(book, book) + ", and " + wasReadable(book)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (readable)'` {
      
      def `should do nothing when all(xs) is readable` {
        (all(List(book)) should (be (readable) or be_== (book))) (readability)
        (all(List(stone)) should (be (readable) or be_== (stone))) (readability)
        (all(List(book)) should (be (readable) or be_== (stone))) (readability)
        
        (all(List(book)) should (be_== (book) or be (readable))) (readability)
        (all(List(book)) should (be_== (stone) or be (readable))) (readability)
        (all(List(stone)) should (be_== (stone) or be (readable))) (readability)
        
        (all(List(book)) should (be (readable) or equal (book))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (all(List(stone)) should (be (readable) or equal (stone))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (all(List(book)) should (be (readable) or equal (stone))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        
        (all(List(book)) should (equal (book) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (all(List(book)) should (equal (stone) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (all(List(stone)) should (equal (stone) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (be_== (book) or be (readable))) (readability)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(stone, book) + ", and " + wasNotReadable(stone), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(stone)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (be (readable) or be_== (book))) (readability)
        }
        assert(caught2.message === Some(allError(wasNotReadable(stone) + ", and " + wasNotEqualTo(stone, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(stone)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (equal (book) or be (readable))) (defaultEquality[Thing{val canRead: Boolean}], readability)
        }
        assert(caught3.message === Some(allError(didNotEqual(stone, book) + ", and " + wasNotReadable(stone), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(stone)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (be (readable) or equal (book))) (readability, defaultEquality[Thing{val canRead: Boolean}])
        }
        assert(caught4.message === Some(allError(wasNotReadable(stone) + ", and " + didNotEqual(stone, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        (all(List(stone)) should (not be readable or not be_== book)) (readability)
        (all(List(book)) should (not be readable or not be_== stone)) (readability)
        (all(List(stone)) should (not be readable or not be_== stone)) (readability)
        
        (all(List(stone)) should (not be_== book or not be readable)) (readability)
        (all(List(stone)) should (not be_== stone or not be readable)) (readability)
        (all(List(book)) should (not be_== stone or not be readable)) (readability)
        
        (all(List(stone)) should (not be readable or not equal book)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (all(List(book)) should (not be readable or not equal stone)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        (all(List(stone)) should (not be readable or not equal stone)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        
        (all(List(stone)) should (not equal book or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (all(List(stone)) should (not equal stone or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        (all(List(book)) should (not equal stone or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (not be_== book or not be readable)) (readability)
        }
        assert(caught1.message === Some(allError(wasEqualTo(book, book) + ", and " + wasReadable(book), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(book)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (not be readable or not be_== book)) (readability)
        }
        assert(caught2.message === Some(allError(wasReadable(book) + ", and " + wasEqualTo(book, book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(book)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (not equal book or not be readable)) (defaultEquality[Thing{val canRead: Boolean}], readability)
        }
        assert(caught3.message === Some(allError(equaled(book, book) + ", and " + wasReadable(book), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(book)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (not be readable or not equal book)) (readability, defaultEquality[Thing{val canRead: Boolean}])
        }
        assert(caught4.message === Some(allError(wasReadable(book) + ", and " + equaled(book, book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
