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

import SharedHelpers.{createTempDirectory, thisLineNumber}
import enablers.Writability
import Matchers._
import exceptions.TestFailedException

class ShouldBeWritableExplicitSpec extends Spec {
  
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
  
  val fileName: String = "ShouldBeWritableExplicitSpec.scala"
    
  def wasNotWritable(left: Any): String = 
    FailureMessages.wasNotWritable(left)
    
  def wasWritable(left: Any): String = 
    FailureMessages.wasWritable(left)
  
  def `book should be writable, stone should not be writable` {
    assert(book.canRead === true)
    assert(stone.canRead === false)
  }
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
  
  object `Writable matcher` {
    
    object `when work with 'file should be (writable)'` {
      
      def `should do nothing when file is writable` {
        (book should be (writable)) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          (stone should be (writable)) (writability)
        }
        assert(caught1.message === Some(wasNotWritable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file should not be writable'` {
      
      def `should do nothing when file is not writable` {
        (stone should not be writable) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is writable` {
        val caught1 = intercept[TestFailedException] {
          (book should not be writable) (writability)
        }
        assert(caught1.message === Some(wasWritable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file shouldBe writable'` {
      
      def `should do nothing when file is writable` {
        (book shouldBe writable) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not writable` {
        val caught1 = intercept[TestFailedException] {
          (stone shouldBe writable) (writability)
        }
        assert(caught1.message === Some(wasNotWritable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file shouldNot be (writable)'` {
      
      def `should do nothing when file is not writable` {
        (stone shouldNot be (writable)) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is writable` {
        val caught1 = intercept[TestFailedException] {
          (book shouldNot be (writable)) (writability)
        }
        assert(caught1.message === Some(wasWritable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should be (writable)'` {
      
      def `should do nothing when all(xs) is writable` {
        (all(List(book)) should be (writable)) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not writable` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should be (writable)) (writability)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should not be writable'` {
      
      def `should do nothing when all(xs) is not writable` {
        (all(List(stone)) should not be writable) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is writable` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should not be writable) (writability)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) shouldBe writable'` {
      
      def `should do nothing when all(xs) is writable` {
        (all(List(book)) shouldBe writable) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not writable` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) shouldBe writable) (writability)
        }
        assert(caught1.message === Some(allError(left1, wasNotWritable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) shouldNot be (writable)'` {
      
      def `should do nothing when all(xs) is not writable` {
        (all(List(stone)) shouldNot be (writable)) (writability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is writable` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) shouldNot be (writable)) (writability)
        }
        assert(caught1.message === Some(allError(left1, wasWritable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
