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

class ListShouldBeEmptyLogicalOrSpec extends Spec with Matchers {
  
  //ADDITIONAL//
  
  val fileName: String = "ListShouldBeEmptyLogicalOrSpec.scala"
  
  def wasEqualTo(left: Any, right: Any): String = 
    FailureMessages("wasEqualTo", left, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages("wasNotEqualTo", leftee, rightee)
  }
    
  def equaled(left: Any, right: Any): String = 
    FailureMessages("equaled", left, right)
    
  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages("didNotEqual", leftee, rightee)
  }
  
  def wasNotEmpty(left: Any): String = 
    FailureMessages("wasNotEmpty", left)
    
  def wasEmpty(left: Any): String = 
    FailureMessages("wasEmpty", left)
    
  def allError(message: String, lineNumber: Int, left: Any): String = {
    val messageWithIndex = UnquotedString("  " + FailureMessages("forAssertionsGenTraversableMessageWithStackDepth", 0, UnquotedString(message), UnquotedString(fileName + ":" + lineNumber)))
    FailureMessages("allShorthandFailed", messageWithIndex, left)
  }
    
  val nonEmptyList = List(1, 2, 3)
  val emptyList = List.empty[Int]
  
  object `Sorted matcher` {
    
    object `when work with 'list should be (empty)'` {
      
      def `should do nothing when list is empty` {
        
        emptyList should (be (empty) or be (emptyList))
        nonEmptyList should (be (empty) or be (nonEmptyList))
        emptyList should (be (empty) or be (nonEmptyList))
        
        emptyList should (be (emptyList) or be (empty))
        emptyList should (be (nonEmptyList) or be (empty))
        nonEmptyList should (be (nonEmptyList) or be (empty))
        
        emptyList should (be (empty) or equal (emptyList))
        nonEmptyList should (be (empty) or equal (nonEmptyList))
        emptyList should (be (empty) or equal (nonEmptyList))
        
        emptyList should (equal (emptyList) or be (empty))
        emptyList should (equal (nonEmptyList) or be (empty))
        nonEmptyList should (equal (nonEmptyList) or be (empty))
      }
      
      def `should throw TestFailedException with correct stack depth when file is not empty` {
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should (be (empty) or be (emptyList))
        }
        assert(caught1.message === Some(wasNotEmpty(nonEmptyList) + ", and " + wasNotEqualTo(nonEmptyList, emptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          nonEmptyList should (be (emptyList) or be (empty))
        }
        assert(caught2.message === Some(wasNotEqualTo(nonEmptyList, emptyList) + ", and " + wasNotEmpty(nonEmptyList)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should (be (empty) or equal (emptyList))
        }
        assert(caught3.message === Some(wasNotEmpty(nonEmptyList) + ", and " + didNotEqual(nonEmptyList, emptyList)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          nonEmptyList should (equal (emptyList) or be (empty))
        }
        assert(caught4.message === Some(didNotEqual(nonEmptyList, emptyList) + ", and " + wasNotEmpty(nonEmptyList)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'file should not be empty'` {
      
      def `should do nothing when file is not empty` {
        nonEmptyList should (not be empty or not be emptyList)
        emptyList should (not be empty or not be nonEmptyList)
        nonEmptyList should (not be empty or not be nonEmptyList)
        
        nonEmptyList should (not be emptyList or not be empty)
        nonEmptyList should (not be nonEmptyList or not be empty)
        emptyList should (not be nonEmptyList or not be empty)
        
        nonEmptyList should (not be empty or not equal emptyList)
        emptyList should (not be empty or not equal nonEmptyList)
        nonEmptyList should (not be empty or not equal nonEmptyList)
        
        nonEmptyList should (not equal emptyList or not be empty)
        nonEmptyList should (not equal nonEmptyList or not be empty)
        emptyList should (not equal nonEmptyList or not be empty)
      }
      
      def `should throw TestFailedException with correct stack depth when file is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyList should (not be empty or not be emptyList)
        }
        assert(caught1.message === Some(wasEmpty(emptyList) + ", and " + wasEqualTo(emptyList, emptyList)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyList should (not be emptyList or not be empty)
        }
        assert(caught2.message === Some(wasEqualTo(emptyList, emptyList) + ", and " + wasEmpty(emptyList)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          emptyList should (not be empty or not equal emptyList)
        }
        assert(caught3.message === Some(wasEmpty(emptyList) + ", and " + equaled(emptyList, emptyList)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          emptyList should (not equal emptyList or not be empty)
        }
        assert(caught4.message === Some(equaled(emptyList, emptyList) + ", and " + wasEmpty(emptyList)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (empty)'` {
      
      def `should do nothing when all(xs) is empty` {
        all(List(emptyList)) should (be (empty) or be (emptyList))
        all(List(nonEmptyList)) should (be (empty) or be (nonEmptyList))
        all(List(emptyList)) should (be (empty) or be (nonEmptyList))
        
        all(List(emptyList)) should (be (emptyList) or be (empty))
        all(List(emptyList)) should (be (nonEmptyList) or be (empty))
        all(List(nonEmptyList)) should (be (nonEmptyList) or be (empty))
        
        all(List(emptyList)) should (be (empty) or equal (emptyList))
        all(List(nonEmptyList)) should (be (empty) or equal (nonEmptyList))
        all(List(emptyList)) should (be (empty) or equal (nonEmptyList))
        
        all(List(emptyList)) should (equal (emptyList) or be (empty))
        all(List(emptyList)) should (equal (nonEmptyList) or be (empty))
        all(List(nonEmptyList)) should (equal (nonEmptyList) or be (empty))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(nonEmptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (emptyList) or be (empty))
        }
        assert(caught1.message === Some(allError(wasNotEqualTo(nonEmptyList, emptyList) + ", and " + wasNotEmpty(nonEmptyList), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(nonEmptyList)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (empty) or be (emptyList))
        }
        assert(caught2.message === Some(allError(wasNotEmpty(nonEmptyList) + ", and " + wasNotEqualTo(nonEmptyList, emptyList), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(nonEmptyList)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (emptyList) or be (empty))
        }
        assert(caught3.message === Some(allError(didNotEqual(nonEmptyList, emptyList) + ", and " + wasNotEmpty(nonEmptyList), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(nonEmptyList)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (empty) or equal (emptyList))
        }
        assert(caught4.message === Some(allError(wasNotEmpty(nonEmptyList) + ", and " + didNotEqual(nonEmptyList, emptyList), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(nonEmptyList)) should (not be empty or not be emptyList)
        all(List(emptyList)) should (not be empty or not be nonEmptyList)
        all(List(nonEmptyList)) should (not be empty or not be nonEmptyList)
        
        all(List(nonEmptyList)) should (not be emptyList or not be empty)
        all(List(nonEmptyList)) should (not be nonEmptyList or not be empty)
        all(List(emptyList)) should (not be nonEmptyList or not be empty)
        
        all(List(nonEmptyList)) should (not be empty or not equal emptyList)
        all(List(emptyList)) should (not be empty or not equal nonEmptyList)
        all(List(nonEmptyList)) should (not be empty or not equal nonEmptyList)
        
        all(List(nonEmptyList)) should (not equal emptyList or not be empty)
        all(List(nonEmptyList)) should (not equal nonEmptyList or not be empty)
        all(List(emptyList)) should (not equal nonEmptyList or not be empty)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(emptyList)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be emptyList or not be empty)
        }
        assert(caught1.message === Some(allError(wasEqualTo(emptyList, emptyList) + ", and " + wasEmpty(emptyList), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some(fileName))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyList)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be empty or not be emptyList)
        }
        assert(caught2.message === Some(allError(wasEmpty(emptyList) + ", and " + wasEqualTo(emptyList, emptyList), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some(fileName))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(emptyList)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal emptyList or not be empty)
        }
        assert(caught3.message === Some(allError(equaled(emptyList, emptyList) + ", and " + wasEmpty(emptyList), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some(fileName))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(emptyList)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be empty or not equal emptyList)
        }
        assert(caught4.message === Some(allError(wasEmpty(emptyList) + ", and " + equaled(emptyList, emptyList), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some(fileName))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
