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
import enablers.Readability
import Matchers._
import exceptions.TestFailedException

class ShouldBeReadableExplicitSpec extends Spec {
  
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
  
  val fileName: String = "ShouldBeReadableExplicitSpec.scala"
    
  def wasNotReadable(left: Any): String = 
    FailureMessages.wasNotReadable(left)
    
  def wasReadable(left: Any): String = 
    FailureMessages.wasReadable(left)
  
  def `book should be readable, stone should not be readable` {
    assert(book.canRead === true)
    assert(stone.canRead === false)
  }
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
  
  object `Readable matcher` {
    
    object `when work with 'file should be (readable)'` {
      
      def `should do nothing when file is readable` {
        (book should be (readable)) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not readable` {
        val caught1 = intercept[TestFailedException] {
          (stone should be (readable)) (readability)
        }
        assert(caught1.message === Some(wasNotReadable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file should not be readable'` {
      
      def `should do nothing when file is not readable` {
        (stone should not be readable) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is readable` {
        val caught1 = intercept[TestFailedException] {
          (book should not be readable) (readability)
        }
        assert(caught1.message === Some(wasReadable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file shouldBe readable'` {
      
      def `should do nothing when file is readable` {
        (book shouldBe readable) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is not readable` {
        val caught1 = intercept[TestFailedException] {
          (stone shouldBe readable) (readability)
        }
        assert(caught1.message === Some(wasNotReadable(stone)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'file shouldNot be (readable)'` {
      
      def `should do nothing when file is not readable` {
        (stone shouldNot be (readable)) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when file is readable` {
        val caught1 = intercept[TestFailedException] {
          (book shouldNot be (readable)) (readability)
        }
        assert(caught1.message === Some(wasReadable(book)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should be (readable)'` {
      
      def `should do nothing when all(xs) is readable` {
        (all(List(book)) should be (readable)) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not readable` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should be (readable)) (readability)
        }
        assert(caught1.message === Some(allError(left1, wasNotReadable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should not be readable'` {
      
      def `should do nothing when all(xs) is not readable` {
        (all(List(stone)) should not be readable) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is readable` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) should not be readable) (readability)
        }
        assert(caught1.message === Some(allError(left1, wasReadable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) shouldBe readable'` {
      
      def `should do nothing when all(xs) is readable` {
        (all(List(book)) shouldBe readable) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not readable` {
        val left1 = List(stone)
        val caught1 = intercept[TestFailedException] {
          (all(left1) shouldBe readable) (readability)
        }
        assert(caught1.message === Some(allError(left1, wasNotReadable(stone), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) shouldNot be (readable)'` {
      
      def `should do nothing when all(xs) is not readable` {
        (all(List(stone)) shouldNot be (readable)) (readability)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is readable` {
        val left1 = List(book)
        val caught1 = intercept[TestFailedException] {
          (all(left1) shouldNot be (readable)) (readability)
        }
        assert(caught1.message === Some(allError(left1, wasReadable(book), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
