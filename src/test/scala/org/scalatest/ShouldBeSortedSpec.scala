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

class ShouldBeSortedSpec extends Spec with Matchers {
  
  def wasNotSorted(left: Any): String = 
    left + " was not sorted"
    
  def wasSorted(left: Any): String = 
    left + " was sorted"
    
  case class Student(name: String, scores: Int)
  implicit val studentOrdering = new Ordering[Student] {
    def compare(a: Student, b: Student) = a.scores compare b.scores
  }
  
  val emptyInts = List.empty[Int]
  val orderedInts = List(1, 2, 3)
  val outOfOrderInts = List(3, 2, 1)
  
  val emptyStudents = List.empty[Student]
  val orderedStudents = List(Student("Student 1", 80), Student("Student 2", 88), Student("Student 3", 90))
  val outOfOrderStudents = List(Student("Student 3", 90), Student("Student 2", 88), Student("Student 1", 80))
  
  object `Sorted matcher` {
    
    object `when work with 'xs should be (sorted)'` {
      
      def `should do nothing when xs is empty` {
        emptyInts should be (sorted)
        emptyStudents should be (sorted)
      }
      
      def `should do nothing when xs is sorted` {
        orderedInts should be (sorted)
        orderedStudents should be (sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          outOfOrderInts should be (sorted)
        }
        assert(caught1.message === Some(wasNotSorted(outOfOrderInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          outOfOrderStudents should be (sorted)
        }
        assert(caught2.message === Some(wasNotSorted(outOfOrderStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `when work with 'xs should not be sorted'` {
      import org.scalatest.enablers.Sortable
      
      def `should do nothing when xs is not sorted` {
        outOfOrderInts should not be sorted
        outOfOrderStudents should not be sorted
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          orderedInts should not be (sorted)
        }
        assert(caught1.message === Some(wasSorted(orderedInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          orderedStudents should not be (sorted)
        }
        assert(caught2.message === Some(wasSorted(orderedStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
  }
  
}