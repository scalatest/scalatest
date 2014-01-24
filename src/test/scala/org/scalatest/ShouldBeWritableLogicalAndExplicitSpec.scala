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

class ShouldBeWritableLogicalAndExplicitSpec extends Spec {
  
  val fileName: String = "ShouldBeWritableLogicalAndExplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotWritable(left: Any): String = 
    FailureMessages("wasNotWritable", left)
    
  def wasWritable(left: Any): String = 
    FailureMessages("wasWritable", left)
    
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
  
  val writability = 
    new Writability[Thing] {
      def isWritable(thing: Thing): Boolean = thing.canRead
    }
  
  object `Writability matcher` {
    
    object `when work with 'file should be (writable)'` {
      
      def `should do nothing when file is writable` {
        (book should (equal (book) and be (writable))) (defaultEquality, writability)
        (book should (be (writable) and equal (book))) (writability, defaultEquality)
        
        (book should (be (book) and be (writable))) (writability)
        (book should (be (writable) and be (book))) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          (stone should (equal (stone) and be (writable))) (defaultEquality, writability)
        }
        assert(caught1.message === Some(equaled(stone, stone) + ", but " + wasNotWritable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (stone should (be (writable) and equal (stone))) (writability, defaultEquality)
        }
        assert(caught2.message === Some(wasNotWritable(stone)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (stone should (be (stone) and be (writable))) (writability)
        }
        assert(caught3.message === Some(wasEqualTo(stone, stone) + ", but " + wasNotWritable(stone)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (stone should (be (writable) and be (stone))) (writability)
        }
        assert(caught4.message === Some(wasNotWritable(stone)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be sorted'` {
      
      def `should do nothing when file is not writable` {
        (stone should (not equal book and not be writable)) (defaultEquality, writability)
        (stone should (not be writable and not equal book)) (writability, defaultEquality)
        
        (stone should (not be book and not be writable)) (writability)
        (stone should (not be writable and not be book)) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          (book should (not equal stone and not be writable)) (defaultEquality, writability)
        }
        assert(caught1.message === Some(didNotEqual(book, stone) + ", but " + wasWritable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (book should (not be writable and not equal stone)) (writability, defaultEquality)
        }
        assert(caught2.message === Some(wasWritable(book)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (book should (not be stone and not be writable)) (writability)
        }
        assert(caught3.message === Some(wasNotEqualTo(book, stone) + ", but " + wasWritable(book)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (book should (not be writable and not be stone)) (writability)
        }
        assert(caught4.message === Some(wasWritable(book)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (writable)'` {
      
      def `should do nothing when all(xs) is writable` {
        (all(List(book)) should (be (book) and be (writable))) (writability)
        (all(List(book)) should (be (writable) and be (book))) (writability)
        
        (all(List(book)) should (equal (book) and be (writable))) (defaultEquality, writability)
        (all(List(book)) should (be (writable) and equal (book))) (writability, defaultEquality)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not writable` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (be (stone) and be (writable))) (writability)
        }
        assert(caught1.message === Some(allError(wasEqualTo(stone, stone) + ", but " + wasNotWritable(stone), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(stone)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (be (writable) and be (stone))) (writability)
        }
        assert(caught2.message === Some(allError(wasNotWritable(stone), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(stone)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (equal (stone) and be (writable))) (defaultEquality, writability)
        }
        assert(caught3.message === Some(allError(equaled(stone, stone) + ", but " + wasNotWritable(stone), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(stone)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (be (writable) and equal (stone))) (writability, defaultEquality)
        }
        assert(caught4.message === Some(allError(wasNotWritable(stone), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be writable'` {
      def `should do nothing when all(xs) is not writable` {
        (all(List(stone)) should (not be writable and not be book)) (writability)
        (all(List(stone)) should (not be book and not be writable)) (writability)
        
        (all(List(stone)) should (not be writable and not equal book)) (writability, defaultEquality)
        (all(List(stone)) should (not equal book and not be writable)) (defaultEquality, writability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is writable` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should (not be stone and not be writable)) (writability)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(book, stone) + ", but " + wasWritable(book), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(book)
        val caught2 = intercept[TestFailedException] {
          (all(left2) should (not be writable and not be stone)) (writability)
        }
        assert(caught2.message === Some(allError(wasWritable(book), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(book)
        val caught3 = intercept[TestFailedException] {
          (all(left3) should (not equal stone and not be writable)) (defaultEquality, writability)
        }
        assert(caught3.message === Some(allError(didNotEqual(book, stone) + ", but " + wasWritable(book), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(book)
        val caught4 = intercept[TestFailedException] {
          (all(left4) should (not be writable and not equal stone)) (writability, defaultEquality)
        }
        assert(caught4.message === Some(allError(wasWritable(book), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
