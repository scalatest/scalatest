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

class ShouldBeEmptyLogicalOrImplicitSpec extends Spec {
  
  val fileName: String = "ShouldBeEmptyLogicalOrImplicitSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasNotEqualTo", left, right)
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String =
    FailureMessages("didNotEqual", left, right)
  
  def wasNotEmpty(left: Any): String = 
    FailureMessages("wasNotEmpty", left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages("wasEmpty", left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
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
  
  object `Sorted matcher` {
    
    object `when work with 'list should be (empty)'` {
      
      def `should do nothing when list is empty` {
        
        emptyThing should (be (empty) or be (emptyThing))
        nonEmptyThing should (be (empty) or be (nonEmptyThing))
        emptyThing should (be (empty) or be (nonEmptyThing))
        
        emptyThing should (be (emptyThing) or be (empty))
        emptyThing should (be (nonEmptyThing) or be (empty))
        nonEmptyThing should (be (nonEmptyThing) or be (empty))
        
        emptyThing should (be (empty) or equal (emptyThing))
        nonEmptyThing should (be (empty) or equal (nonEmptyThing))
        emptyThing should (be (empty) or equal (nonEmptyThing))
        
        emptyThing should (equal (emptyThing) or be (empty))
        emptyThing should (equal (nonEmptyThing) or be (empty))
        nonEmptyThing should (equal (nonEmptyThing) or be (empty))
      }
      
      def `should throw TestFailedException with correct stack depth when file is not empty` {
        val caught1 = intercept[TestFailedException] {
          nonEmptyThing should (be (empty) or be (emptyThing))
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyThing) + ", and " + wasNotEqualTo(nonEmptyThing, emptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          nonEmptyThing should (be (emptyThing) or be (empty))
        }
        assert(caught2.message === Some(wasNotEqualTo(nonEmptyThing, emptyThing) + ", and " + wasNotEmpty(nonEmptyThing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          nonEmptyThing should (be (empty) or equal (emptyThing))
        }
        assert(caught3.message === Some(wasNotEmpty(nonEmptyThing) + ", and " + didNotEqual(nonEmptyThing, emptyThing)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          nonEmptyThing should (equal (emptyThing) or be (empty))
        }
        assert(caught4.message === Some(didNotEqual(nonEmptyThing, emptyThing) + ", and " + wasNotEmpty(nonEmptyThing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be empty'` {
      
      def `should do nothing when file is not empty` {
        nonEmptyThing should (not be empty or not be emptyThing)
        emptyThing should (not be empty or not be nonEmptyThing)
        nonEmptyThing should (not be empty or not be nonEmptyThing)
        
        nonEmptyThing should (not be emptyThing or not be empty)
        nonEmptyThing should (not be nonEmptyThing or not be empty)
        emptyThing should (not be nonEmptyThing or not be empty)
        
        nonEmptyThing should (not be empty or not equal emptyThing)
        emptyThing should (not be empty or not equal nonEmptyThing)
        nonEmptyThing should (not be empty or not equal nonEmptyThing)
        
        nonEmptyThing should (not equal emptyThing or not be empty)
        nonEmptyThing should (not equal nonEmptyThing or not be empty)
        emptyThing should (not equal nonEmptyThing or not be empty)
      }
      
      def `should throw TestFailedException with correct stack depth when file is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyThing should (not be empty or not be emptyThing)
        }
        assert(caught1.message === Some(wasEmpty(emptyThing) + ", and " + wasEqualTo(emptyThing, emptyThing)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyThing should (not be emptyThing or not be empty)
        }
        assert(caught2.message === Some(wasEqualTo(emptyThing, emptyThing) + ", and " + wasEmpty(emptyThing)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          emptyThing should (not be empty or not equal emptyThing)
        }
        assert(caught3.message === Some(wasEmpty(emptyThing) + ", and " + equaled(emptyThing, emptyThing)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          emptyThing should (not equal emptyThing or not be empty)
        }
        assert(caught4.message === Some(equaled(emptyThing, emptyThing) + ", and " + wasEmpty(emptyThing)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (empty)'` {
      
      def `should do nothing when all(xs) is empty` {
        all(List(emptyThing)) should (be (empty) or be (emptyThing))
        all(List(nonEmptyThing)) should (be (empty) or be (nonEmptyThing))
        all(List(emptyThing)) should (be (empty) or be (nonEmptyThing))
        
        all(List(emptyThing)) should (be (emptyThing) or be (empty))
        all(List(emptyThing)) should (be (nonEmptyThing) or be (empty))
        all(List(nonEmptyThing)) should (be (nonEmptyThing) or be (empty))
        
        all(List(emptyThing)) should (be (empty) or equal (emptyThing))
        all(List(nonEmptyThing)) should (be (empty) or equal (nonEmptyThing))
        all(List(emptyThing)) should (be (empty) or equal (nonEmptyThing))
        
        all(List(emptyThing)) should (equal (emptyThing) or be (empty))
        all(List(emptyThing)) should (equal (nonEmptyThing) or be (empty))
        all(List(nonEmptyThing)) should (equal (nonEmptyThing) or be (empty))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(nonEmptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (emptyThing) or be (empty))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(nonEmptyThing, emptyThing) + ", and " + wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nonEmptyThing)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (empty) or be (emptyThing))
        }
        assert(caught2.message === Some(allError(wasNotEmpty(nonEmptyThing) + ", and " + wasNotEqualTo(nonEmptyThing, emptyThing), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nonEmptyThing)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (emptyThing) or be (empty))
        }
        assert(caught3.message === Some(allError(didNotEqual(nonEmptyThing, emptyThing) + ", and " + wasNotEmpty(nonEmptyThing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nonEmptyThing)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (empty) or equal (emptyThing))
        }
        assert(caught4.message === Some(allError(wasNotEmpty(nonEmptyThing) + ", and " + didNotEqual(nonEmptyThing, emptyThing), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(nonEmptyThing)) should (not be empty or not be emptyThing)
        all(List(emptyThing)) should (not be empty or not be nonEmptyThing)
        all(List(nonEmptyThing)) should (not be empty or not be nonEmptyThing)
        
        all(List(nonEmptyThing)) should (not be emptyThing or not be empty)
        all(List(nonEmptyThing)) should (not be nonEmptyThing or not be empty)
        all(List(emptyThing)) should (not be nonEmptyThing or not be empty)
        
        all(List(nonEmptyThing)) should (not be empty or not equal emptyThing)
        all(List(emptyThing)) should (not be empty or not equal nonEmptyThing)
        all(List(nonEmptyThing)) should (not be empty or not equal nonEmptyThing)
        
        all(List(nonEmptyThing)) should (not equal emptyThing or not be empty)
        all(List(nonEmptyThing)) should (not equal nonEmptyThing or not be empty)
        all(List(emptyThing)) should (not equal nonEmptyThing or not be empty)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(emptyThing)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be emptyThing or not be empty)
        }
        assert(caught1.message === Some(allError(wasEqualTo(emptyThing, emptyThing) + ", and " + wasEmpty(emptyThing), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyThing)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be empty or not be emptyThing)
        }
        assert(caught2.message === Some(allError(wasEmpty(emptyThing) + ", and " + wasEqualTo(emptyThing, emptyThing), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(emptyThing)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal emptyThing or not be empty)
        }
        assert(caught3.message === Some(allError(equaled(emptyThing, emptyThing) + ", and " + wasEmpty(emptyThing), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(emptyThing)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be empty or not equal emptyThing)
        }
        assert(caught4.message === Some(allError(wasEmpty(emptyThing) + ", and " + equaled(emptyThing, emptyThing), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
