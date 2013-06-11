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
import org.scalatest.enablers.Sortable

class ShouldBeSortedLogicalAndSpec extends Spec with Matchers {
  
  def wasEqualTo(left: Any, right: Any): String = 
    left + " was equal to " + right
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    left + " was not equal to " + right
    
  def equaled(left: Any, right: Any): String = 
    left + " equaled " + right
    
  def didNotEqual(left: Any, right: Any): String = 
    left + " did not equal " + right
  
  def wasNotSorted(left: Any): String = 
    left + " was not sorted"
    
  def wasSorted(left: Any): String = 
    left + " was sorted"
    
  def allInspectionFailed(idx: Int, message: String, lineNumber:Int, left: Any) = 
    "'all' inspection failed, because: \n" + 
    "  at index " + idx + ", " + message + " (ShouldBeSortedLogicalAndSpec.scala:" + lineNumber + ") \n" + 
    "in " + left
    
  case class Student(name: String, scores: Int)
  implicit val studentOrdering = new Ordering[Student] {
    def compare(a: Student, b: Student) = a.scores compare b.scores
  }
  
  val orderedInts = List(1, 2, 3)
  val outOfOrderInts = List(3, 2, 1)
  
  val orderedStudents = List(Student("Student 1", 80), Student("Student 2", 88), Student("Student 3", 90))
  val outOfOrderStudents = List(Student("Student 3", 90), Student("Student 2", 88), Student("Student 1", 80))
  
  val notSortedForInt = new Sortable[List[Int]] {
    def isSorted(list: List[Int]): Boolean = list != list.sortWith(_ < _)
  }
  
  val notSortedForStudent = new Sortable[List[Student]] {
    def isSorted(list: List[Student]): Boolean = list != list.sortWith(_.scores < _.scores)
  }
  
  object `Sorted matcher` {
    
    object `when work with 'xs should be (sorted)'` {
      
      def `should do nothing when xs is sorted` {
        orderedStudents should (be (sorted) and be (sorted))
        orderedStudents should (be (sorted) and be (orderedStudents))
        orderedStudents should (be (orderedStudents) and be (sorted))
        
        orderedStudents should (be (sorted) and be (sorted))
        orderedStudents should (be (sorted) and equal (orderedStudents))
        orderedStudents should (equal (orderedStudents) and be (sorted))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          outOfOrderInts should (be (outOfOrderInts) and be (sorted))
        }
        assert(caught1.message === Some(wasEqualTo(outOfOrderInts, outOfOrderInts) + ", but " + wasNotSorted(outOfOrderInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) and be (outOfOrderInts))
        }
        assert(caught2.message === Some(wasNotSorted(outOfOrderInts)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (outOfOrderStudents should (equal (outOfOrderStudents) and be (sorted)))
        }
        assert(caught3.message === Some(equaled(outOfOrderStudents, outOfOrderStudents) + ", but " + wasNotSorted(outOfOrderStudents)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (outOfOrderStudents should (be (sorted) and equal (outOfOrderStudents)))
        }
        assert(caught4.message === Some(wasNotSorted(outOfOrderStudents)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }      
    }
    
    object `when work with 'xs should not be sorted'` {
      
      def `should do nothing when xs is not sorted` {
        outOfOrderInts should (not be sorted and not be outOfOrderStudents)
        outOfOrderInts should (not be outOfOrderStudents and not be sorted)
        
        outOfOrderStudents should (not be sorted and not equal outOfOrderInts)
        outOfOrderStudents should (not equal outOfOrderInts and not be sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          orderedInts should (not be outOfOrderStudents and not be sorted)
        }
        assert(caught1.message === Some(wasNotEqualTo(orderedInts, outOfOrderStudents) + ", but " + wasSorted(orderedInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          orderedInts should (not be sorted and not be outOfOrderStudents)
        }
        assert(caught2.message === Some(wasSorted(orderedInts)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          orderedStudents should (not equal outOfOrderInts and not be sorted)
        }
        assert(caught3.message === Some(didNotEqual(orderedStudents, outOfOrderInts) + ", but " + wasSorted(orderedStudents)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          orderedStudents should (not be sorted and not equal outOfOrderInts)
        }
        assert(caught4.message === Some(wasSorted(orderedStudents)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should be (sorted)'` {
      
      def `should do nothing when xs is sorted` {
        all(List(orderedStudents)) should (be (sorted) and be (sorted))
        all(List(orderedStudents)) should (be (sorted) and be (orderedStudents))
        all(List(orderedStudents)) should (be (orderedStudents) and be (sorted))
        
        all(List(orderedStudents)) should (be (sorted) and be (sorted))
        all(List(orderedStudents)) should (be (sorted) and equal (orderedStudents))
        all(List(orderedStudents)) should (equal (orderedStudents) and be (sorted))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(outOfOrderInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (outOfOrderInts) and be (sorted))
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasEqualTo(outOfOrderInts, outOfOrderInts) + ", but " + wasNotSorted(outOfOrderInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(outOfOrderInts)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (sorted) and be (outOfOrderInts))
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderInts), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(outOfOrderStudents)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (outOfOrderStudents) and be (sorted))
        }
        assert(caught3.message === Some(allInspectionFailed(0, equaled(outOfOrderStudents, outOfOrderStudents) + ", but " + wasNotSorted(outOfOrderStudents), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(outOfOrderStudents)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (sorted) and equal (outOfOrderStudents))
        }
        assert(caught4.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderStudents), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(outOfOrderInts)) should (not be sorted and not be outOfOrderStudents)
        all(List(outOfOrderInts)) should (not be outOfOrderStudents and not be sorted)
        
        all(List(outOfOrderStudents)) should (not be sorted and not equal outOfOrderInts)
        all(List(outOfOrderStudents)) should (not equal outOfOrderInts and not be sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(orderedInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be outOfOrderStudents and not be sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasNotEqualTo(orderedInts, outOfOrderStudents) + ", but " + wasSorted(orderedInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(orderedInts)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be sorted and not be outOfOrderStudents)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(orderedInts), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(orderedStudents)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal outOfOrderInts and not be sorted)
        }
        assert(caught3.message === Some(allInspectionFailed(0, didNotEqual(orderedStudents, outOfOrderInts) + ", but " + wasSorted(orderedStudents), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(orderedStudents)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be sorted and not equal outOfOrderInts)
        }
        assert(caught4.message === Some(allInspectionFailed(0, wasSorted(orderedStudents), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    // shouldBe and shouldNot does not support logical expression yet.
  }
  
}