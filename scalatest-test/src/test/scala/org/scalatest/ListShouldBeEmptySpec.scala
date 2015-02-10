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
import Matchers._
import exceptions.TestFailedException

class ListShouldBeEmptySpec extends Spec {
  
  //ADDITIONAL//
  
  val nonEmptyList = List(1, 2, 3)
  val emptyList = List.empty[Int]
  
  val fileName: String = "ListShouldBeEmptySpec.scala"
    
  def wasNotEmpty(left: Any): String = 
    FailureMessages("wasNotEmpty", left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages("wasEmpty", left)
  
  def allError(left: Any, message: String, lineNumber: Int): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
  
  object `Empty matcher` {
    
    object `when work with 'list should be (empty)'` {
      
      def `should do nothing when list is empty` {
        emptyList should be (empty)
      }
      
      def `should throw TestFailedException with correct stack depth when list is not empty` {
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should be (empty)
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'list should not be empty'` {
      
      def `should do nothing when list is not empty` {
        nonEmptyList should not be empty
      }
      
      def `should throw TestFailedException with correct stack depth when list is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyList should not be empty
        }
        assert(caught1.message === Some(wasEmpty(emptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'list shouldBe empty'` {
      
      def `should do nothing when list is empty` {
        emptyList shouldBe empty
      }
      
      def `should throw TestFailedException with correct stack depth when list is not empty` {
        val caught1 = intercept[TestFailedException] {
          nonEmptyList shouldBe empty
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'list shouldNot be (empty)'` {
      
      def `should do nothing when list is not empty` {
        nonEmptyList shouldNot be (empty)
      }
      
      def `should throw TestFailedException with correct stack depth when list is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyList shouldNot be (empty)
        }
        assert(caught1.message === Some(wasEmpty(emptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should be (empty)'` {
      
      def `should do nothing when all(xs) is empty` {
        all(List(emptyList)) should be (empty)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not empty` {
        val left1 = List(nonEmptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (empty)
        }
        assert(caught1.message === Some(allError(left1, wasNotEmpty(nonEmptyList), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) should not be empty'` {
      
      def `should do nothing when all(xs) is not empty` {
        all(List(nonEmptyList)) should not be empty
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is empty` {
        val left1 = List(emptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be empty
        }
        assert(caught1.message === Some(allError(left1, wasEmpty(emptyList), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'all(xs) shouldBe empty'` {
      
      def `should do nothing when all(xs) is empty` {
        all(List(emptyList)) shouldBe empty
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not empty` {
        val left1 = List(nonEmptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe empty
        }
        assert(caught1.message === Some(allError(left1, wasNotEmpty(nonEmptyList), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) shouldNot be (empty)'` {
      
      def `should do nothing when all(xs) is not empty` {
        all(List(nonEmptyList)) shouldNot be (empty)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is empty` {
        val left1 = List(emptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (empty)
        }
        assert(caught1.message === Some(allError(left1, wasEmpty(emptyList), thisLineNumber - 2)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
  }
  
}
