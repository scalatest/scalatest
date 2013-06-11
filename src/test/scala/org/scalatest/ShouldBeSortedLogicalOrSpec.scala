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

class ShouldBeSortedLogicalOrSpec extends Spec with Matchers {
  
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
    "  at index " + idx + ", " + message + " (ShouldBeSortedLogicalOrSpec.scala:" + lineNumber + ") \n" + 
    "in " + left
    
  case class Student(name: String, scores: Int)
  implicit val studentOrdering = new Ordering[Student] {
    def compare(a: Student, b: Student) = a.scores compare b.scores
  }
  
  val orderedInts = List(1, 2, 3)
  val outOfOrderInts = List(3, 2, 1)
  
  val orderedStudents = List(Student("Student 1", 80), Student("Student 2", 88), Student("Student 3", 90))
  val outOfOrderStudents = List(Student("Student 3", 90), Student("Student 2", 88), Student("Student 1", 80))
  
  val trueSortable = 
    new Sortable[List[Int]] {
      def isSorted(o: List[Int]) = true
    }
  
  val falseSortable = 
    new Sortable[List[Int]] {
      def isSorted(o: List[Int]) = false
    }
  
  object `Sorted matcher` {
    
    object `when work with 'xs should be (sorted)'` {
      
      def `should do nothing when xs is sorted` {
        orderedStudents should (be (sorted) or be (sorted))
        orderedStudents should (be (sorted) or be (orderedInts))
        orderedStudents should (be (orderedInts) or be (sorted))
        
        orderedStudents should (be (sorted) or be (sorted))
        orderedStudents should (be (sorted) or equal (orderedInts))
        orderedStudents should (equal (orderedStudents) or be (sorted))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          outOfOrderInts should (be (outOfOrderStudents) or be (sorted))
        }
        assert(caught1.message === Some(wasNotEqualTo(outOfOrderInts, outOfOrderStudents) + ", and " + wasNotSorted(outOfOrderInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) or be (outOfOrderStudents))
        }
        assert(caught2.message === Some(wasNotSorted(outOfOrderInts) + ", and " + wasNotEqualTo(outOfOrderInts, outOfOrderStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (outOfOrderStudents should (equal (outOfOrderInts) or be (sorted)))
        }
        assert(caught3.message === Some(didNotEqual(outOfOrderStudents, outOfOrderInts) + ", and " + wasNotSorted(outOfOrderStudents)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (outOfOrderStudents should (be (sorted) or equal (outOfOrderInts)))
        }
        assert(caught4.message === Some(wasNotSorted(outOfOrderStudents) + ", and " + didNotEqual(outOfOrderStudents, outOfOrderInts)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) or be (orderedInts))
        }
        implicit val imp = trueSortable
        outOfOrderInts should (be (sorted) or be (orderedInts))
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) or equal (orderedInts))
        }
        (outOfOrderInts should (be (sorted) or equal (orderedInts))) (trueSortable, defaultEquality)
      }
    }
    
    object `when work with 'xs should not be sorted'` {
      
      def `should do nothing when xs is not sorted` {
        outOfOrderInts should (not be sorted or not be outOfOrderInts)
        outOfOrderInts should (not be outOfOrderInts or not be sorted)
        
        outOfOrderStudents should (not be sorted or not equal outOfOrderStudents)
        outOfOrderStudents should (not equal outOfOrderStudents or not be sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          orderedInts should (not be orderedInts or not be sorted)
        }
        assert(caught1.message === Some(wasEqualTo(orderedInts, orderedInts) + ", and " + wasSorted(orderedInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          orderedInts should (not be sorted or not be orderedInts)
        }
        assert(caught2.message === Some(wasSorted(orderedInts) + ", and " + wasEqualTo(orderedInts, orderedInts)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          orderedStudents should (not equal orderedStudents or not be sorted)
        }
        assert(caught3.message === Some(equaled(orderedStudents, orderedStudents) + ", and " + wasSorted(orderedStudents)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          orderedStudents should (not be sorted or not equal orderedStudents)
        }
        assert(caught4.message === Some(wasSorted(orderedStudents) + ", and " + equaled(orderedStudents, orderedStudents)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          orderedInts should (not be (sorted) or not be (orderedInts))
        }
        implicit val imp = falseSortable
        orderedInts should (not be (sorted) or not be (orderedInts))
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          orderedInts should (not be (sorted) or not equal (orderedInts))
        }
        (orderedInts should (not be (sorted) or not equal (orderedInts))) (falseSortable, defaultEquality)
      }
    }
    
    object `when work with 'all(xs) should be (sorted)'` {
      
      def `should do nothing when xs is sorted` {
        all(List(orderedStudents)) should (be (sorted) or be (sorted))
        all(List(orderedStudents)) should (be (sorted) or be (orderedInts))
        all(List(orderedStudents)) should (be (orderedInts) or be (sorted))
        
        all(List(orderedStudents)) should (be (sorted) or be (sorted))
        all(List(orderedStudents)) should (be (sorted) or equal (orderedInts))
        all(List(orderedStudents)) should (equal (orderedInts) or be (sorted))
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(outOfOrderInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (be (orderedInts) or be (sorted))
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasNotEqualTo(outOfOrderInts, orderedInts) + ", and " + wasNotSorted(outOfOrderInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(outOfOrderInts)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (be (sorted) or be (orderedInts))
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderInts) + ", and " + wasNotEqualTo(outOfOrderInts, orderedInts), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(outOfOrderStudents)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (equal (orderedStudents) or be (sorted))
        }
        assert(caught3.message === Some(allInspectionFailed(0, didNotEqual(outOfOrderStudents, orderedStudents) + ", and " + wasNotSorted(outOfOrderStudents), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(outOfOrderStudents)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (be (sorted) or equal (orderedStudents))
        }
        assert(caught4.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderStudents) + ", and " + didNotEqual(outOfOrderStudents, orderedStudents), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should (be (sorted) or be (orderedInts))
        }
        implicit val imp = trueSortable
        all(List(outOfOrderInts)) should (be (sorted) or be (orderedInts))
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should (be (sorted) or equal (orderedInts))
        }
        (all(List(outOfOrderInts)) should (be (sorted) or equal (orderedInts))) (trueSortable, defaultEquality)
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      def `should do nothing when xs is not sorted` {
        all(List(outOfOrderInts)) should (not be sorted or not be orderedStudents)
        all(List(outOfOrderInts)) should (not be orderedStudents or not be sorted)
        
        all(List(outOfOrderStudents)) should (not be sorted or not equal orderedInts)
        all(List(outOfOrderStudents)) should (not equal orderedInts or not be sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(orderedInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should (not be orderedInts or not be sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasEqualTo(orderedInts, orderedInts) + ", and " + wasSorted(orderedInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(orderedInts)
        val caught2 = intercept[TestFailedException] {
          all(left2) should (not be sorted or not be orderedInts)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(orderedInts) + ", and " + wasEqualTo(orderedInts, orderedInts), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left3 = List(orderedStudents)
        val caught3 = intercept[TestFailedException] {
          all(left3) should (not equal orderedStudents or not be sorted)
        }
        assert(caught3.message === Some(allInspectionFailed(0, equaled(orderedStudents, orderedStudents) + ", and " + wasSorted(orderedStudents), thisLineNumber - 2, left3)))
        assert(caught3.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left4 = List(orderedStudents)
        val caught4 = intercept[TestFailedException] {
          all(left4) should (not be sorted or not equal orderedStudents)
        }
        assert(caught4.message === Some(allInspectionFailed(0, wasSorted(orderedStudents) + ", and " + equaled(orderedStudents, orderedStudents), thisLineNumber - 2, left4)))
        assert(caught4.failedCodeFileName === Some("ShouldBeSortedLogicalOrSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(orderedInts)) should (not be (sorted) or not be (orderedInts))
        }
        implicit val imp = falseSortable
        all(List(orderedInts)) should (not be (sorted) or not be (orderedInts))
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(orderedInts)) should (not be (sorted) or not equal (orderedInts))
        }
        (all(List(orderedInts)) should (not be (sorted) or not equal (orderedInts))) (falseSortable, defaultEquality)
      }
    }
    
    // shouldBe and shouldNot does not support logical expression yet.
  }
  
}