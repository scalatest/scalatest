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
import enablers.Emptiness
import Matchers._
import exceptions.TestFailedException

class ShouldBeEmptyLogicalAndImplicitSpec extends Spec {
  
  val fileName: String = "ShouldBeEmptyLogicalAndImplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasEqualTo(left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages.wasNotEqualTo(left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages.equaled(left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages.didNotEqual(left, right)
  
  def wasNotEmpty(left: Any): String = 
    FailureMessages.wasNotEmpty(left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages.wasEmpty(left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages.forAssertionsGenTraversableMessageWithStackDepth(0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages.allShorthandFailed(messageWithIndex, left)
  }
    
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
  
  object `Emptiness matcher` {
    
    object `when work with 'list should be (empty)'` {
      
      def `should do nothing when list is empty` {
        emptyThing should (equal (emptyThing) and be (empty))
        emptyThing should (be (empty) and equal (emptyThing))
        
        emptyThing should (be_== (emptyThing) and be (empty))
        emptyThing should (be (empty) and be_== (emptyThing))
      }
      
      def `should throw TestFailedException with correct stack depth when list is not empty` {
        val caught1 = intercept[TestFailedException] {
          nonEmptyThing should (equal (nonEmptyThing) and be (empty))
        }
        assert(caught1.message === Some(equaled(nonEmptyThing, nonEmptyThing) + ", but " + wasNotEmpty(nonEmptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          nonEmptyThing should (be (empty) and equal (nonEmptyThing))
        }
        assert(caught2.message === Some(wasNotEmpty(nonEmptyThing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          nonEmptyThing should (be_== (nonEmptyThing) and be (empty))
        }
        assert(caught3.message === Some(wasEqualTo(nonEmptyThing, nonEmptyThing) + ", but " + wasNotEmpty(nonEmptyThing)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          nonEmptyThing should (be (empty) and be_== (nonEmptyThing))
        }
        assert(caught4.message === Some(wasNotEmpty(nonEmptyThing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'list should not be empty'` {
      
      def `should do nothing when file is not empty` {
        nonEmptyThing should (not equal emptyThing and not be empty)
        nonEmptyThing should (not be empty and not equal emptyThing)
        
        nonEmptyThing should (not be_== emptyThing and not be empty)
        nonEmptyThing should (not be empty and not be_== emptyThing)
      }
      
      def `should throw TestFailedException with correct stack depth when list is not empty` {
        val caught1 = intercept[TestFailedException] {
          emptyThing should (not equal nonEmptyThing and not be empty)
        }
        assert(caught1.message === Some(didNotEqual(emptyThing, nonEmptyThing) + ", but " + wasEmpty(emptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyThing should (not be empty and not equal nonEmptyThing)
        }
        assert(caught2.message === Some(wasEmpty(emptyThing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          emptyThing should (not be_== nonEmptyThing and not be empty)
        }
        assert(caught3.message === Some(wasNotEqualTo(emptyThing, nonEmptyThing) + ", but " + wasEmpty(emptyThing)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          emptyThing should (not be empty and not be_== nonEmptyThing)
        }
        assert(caught4.message === Some(wasEmpty(emptyThing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (empty)'` {
      
      def `should do nothing when all(xs) is empty` {
        all(List(emptyThing)) should (be_== (emptyThing) and be (empty))
        all(List(emptyThing)) should (be (empty) and be_== (emptyThing))
        
        all(List(emptyThing)) should (equal (emptyThing) and be (empty))
        all(List(emptyThing)) should (be (empty) and equal (emptyThing))
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is not empty` {
        val left1 = List(nonEmptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be_== (nonEmptyThing) and be (empty))
        }
        assert(caught1.message === Some(allError(wasEqualTo(nonEmptyThing, nonEmptyThing) + ", but " + wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nonEmptyThing)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (empty) and be_== (nonEmptyThing))
        }
        assert(caught2.message === Some(allError(wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nonEmptyThing)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (nonEmptyThing) and be (empty))
        }
        assert(caught3.message === Some(allError(equaled(nonEmptyThing, nonEmptyThing) + ", but " + wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nonEmptyThing)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (empty) and equal (nonEmptyThing))
        }
        assert(caught4.message === Some(allError(wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be empty'` {
      def `should do nothing when all(xs) is not empty` {
        all(List(nonEmptyThing)) should (not be empty and not be_== emptyThing)
        all(List(nonEmptyThing)) should (not be_== emptyThing and not be empty)
        
        all(List(nonEmptyThing)) should (not be empty and not equal emptyThing)
        all(List(nonEmptyThing)) should (not equal emptyThing and not be empty)
      }
      
      def `should throw TestFailedException with correct stack depth when all(xs) is empty` {
        val left1 = List(emptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be_== nonEmptyThing and not be empty)
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(emptyThing, nonEmptyThing) + ", but " + wasEmpty(emptyThing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyThing)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be empty and not be_== nonEmptyThing)
        }
        assert(caught2.message === Some(allError(wasEmpty(emptyThing), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(emptyThing)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal nonEmptyThing and not be empty)
        }
        assert(caught3.message === Some(allError(didNotEqual(emptyThing, nonEmptyThing) + ", but " + wasEmpty(emptyThing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(emptyThing)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be empty and not equal nonEmptyThing)
        }
        assert(caught4.message === Some(allError(wasEmpty(emptyThing), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
