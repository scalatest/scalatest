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
import enablers.Sortable
import FailureMessages.decorateToStringValue

class ShouldBeSortedSpec extends Spec with Matchers {
  
  //ADDITIONAL//
  
  def wasNotSorted(left: Any): String = 
    decorateToStringValue(left) + " was not sorted"
    
  def wasSorted(left: Any): String = 
    decorateToStringValue(left) + " was sorted"
    
  def allInspectionFailed(idx: Int, message: String, lineNumber:Int, left: Any) = 
    "'all' inspection failed, because: \n" + 
    "  at index " + idx + ", " + message + " (ShouldBeSortedSpec.scala:" + lineNumber + ") \n" + 
    "in " + decorateToStringValue(left)
    
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
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          outOfOrderInts should be (sorted)
        }
        implicit val imp = trueSortable
        outOfOrderInts should be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          outOfOrderInts should be (sorted)
        }
        outOfOrderInts should be (sorted) (trueSortable)
      }
      
    }
    
    object `when work with 'xs should not be sorted'` {
      import org.scalatest.enablers.Sortable
      
      def `should throw TestFailedException wht correct stack depth when xs is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyInts should not be sorted
        }
        assert(caught1.message === Some(wasSorted(emptyInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyStudents should not be sorted
        }
        assert(caught2.message === Some(wasSorted(emptyStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
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
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          orderedInts should not be (sorted)
        }
        implicit val imp = falseSortable
        orderedInts should not be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          orderedInts should not be sorted
        }
        (orderedInts should not be (sorted)) (falseSortable)
      }
    }
    
    object `when work with 'xs shouldBe (sorted)'` {
      
      def `should do nothing when xs is empty` {
        emptyInts shouldBe sorted
        emptyStudents shouldBe (sorted)
      }
      
      def `should do nothing when xs is sorted` {
        orderedInts shouldBe sorted
        orderedStudents shouldBe sorted
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          outOfOrderInts shouldBe (sorted)
        }
        assert(caught1.message === Some(wasNotSorted(outOfOrderInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          outOfOrderStudents shouldBe (sorted)
        }
        assert(caught2.message === Some(wasNotSorted(outOfOrderStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          outOfOrderInts shouldBe sorted
        }
        implicit val imp = trueSortable
        outOfOrderInts shouldBe sorted
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          outOfOrderInts shouldBe sorted
        }
        (outOfOrderInts shouldBe (sorted)) (trueSortable)
      }
      
    }
    
    object `when work with 'xs shouldNot be (sorted)'` {
      import org.scalatest.enablers.Sortable
      
      def `should throw TestFailedException wht correct stack depth when xs is empty` {
        val caught1 = intercept[TestFailedException] {
          emptyInts shouldNot be (sorted)
        }
        assert(caught1.message === Some(wasSorted(emptyInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          emptyStudents shouldNot be (sorted)
        }
        assert(caught2.message === Some(wasSorted(emptyStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when xs is not sorted` {
        outOfOrderInts shouldNot be (sorted)
        outOfOrderStudents shouldNot be (sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val caught1 = intercept[TestFailedException] {
          orderedInts shouldNot be (sorted)
        }
        assert(caught1.message === Some(wasSorted(orderedInts)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          orderedStudents shouldNot be (sorted)
        }
        assert(caught2.message === Some(wasSorted(orderedStudents)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          orderedInts shouldNot be (sorted)
        }
        implicit val imp = falseSortable
        orderedInts shouldNot be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          orderedInts shouldNot be (sorted)
        }
        orderedInts shouldNot be (sorted) (falseSortable)
      }
    }
    
    object `when work with 'all(xs) should be (sorted)'` {
      
      def `should do nothing when xs is empty` {
        all(List(emptyInts)) should be (sorted)
        all(List(emptyStudents)) should be (sorted)
      }
      
      def `should do nothing when xs is sorted` {
        all(List(orderedInts)) should be (sorted)
        all(List(orderedStudents)) should be (sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(outOfOrderInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should be (sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(outOfOrderStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) should be (sorted)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should be (sorted)
        }
        implicit val imp = trueSortable
        all(List(outOfOrderInts)) should be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should be (sorted)
        }
        all(List(outOfOrderInts)) should be (sorted) (trueSortable)
      }
    }
    
    object `when work with 'all(xs) should not be sorted'` {
      import org.scalatest.enablers.Sortable
      
      def `should throw TestFailedException wht correct stack depth when xs is empty` {
        val left1 = List(emptyInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be sorted
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasSorted(emptyInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) should not be sorted
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(emptyStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when xs is not sorted` {
        all(List(outOfOrderInts)) should not be sorted
        all(List(outOfOrderStudents)) should not be sorted
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(orderedInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) should not be (sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasSorted(orderedInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(orderedStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) should not be (sorted)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(orderedStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(orderedInts)) should not be (sorted)
        }
        implicit val imp = falseSortable
        all(List(orderedInts)) should not be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(orderedInts)) should not be sorted
        }
        (all(List(orderedInts)) should not be (sorted)) (falseSortable)
      }
    }
    
    object `when work with 'all(xs) shouldBe (sorted)'` {
      
      def `should do nothing when xs is empty` {
        all(List(emptyInts)) shouldBe sorted
        all(List(emptyStudents)) shouldBe (sorted)
      }
      
      def `should do nothing when xs is sorted` {
        all(List(orderedInts)) shouldBe sorted
        all(List(orderedStudents)) shouldBe sorted
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(outOfOrderInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldBe (sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(outOfOrderStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) shouldBe (sorted)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) shouldBe sorted
        }
        implicit val imp = trueSortable
        all(List(outOfOrderInts)) shouldBe sorted
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) shouldBe sorted
        }
        (all(List(outOfOrderInts)) shouldBe (sorted)) (trueSortable)
      }
      
    }
    
    object `when work with 'all(xs) shouldNot be (sorted)'` {
      import org.scalatest.enablers.Sortable
      
      def `should throw TestFailedException wht correct stack depth when xs is empty` {
        val left1 = List(emptyInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasSorted(emptyInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(emptyStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) shouldNot be (sorted)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(emptyStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when xs is not sorted` {
        all(List(outOfOrderInts)) shouldNot be (sorted)
        all(List(outOfOrderStudents)) shouldNot be (sorted)
      }
      
      def `should throw TestFailedException with correct stack depth when xs is not sorted` {
        val left1 = List(orderedInts)
        val caught1 = intercept[TestFailedException] {
          all(left1) shouldNot be (sorted)
        }
        assert(caught1.message === Some(allInspectionFailed(0, wasSorted(orderedInts), thisLineNumber - 2, left1)))
        assert(caught1.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left2 = List(orderedStudents)
        val caught2 = intercept[TestFailedException] {
          all(left2) shouldNot be (sorted)
        }
        assert(caught2.message === Some(allInspectionFailed(0, wasSorted(orderedStudents), thisLineNumber - 2, left2)))
        assert(caught2.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should use implicit Sortable when available` {
        intercept[TestFailedException] {
          all(List(orderedInts)) shouldNot be (sorted)
        }
        implicit val imp = falseSortable
        all(List(orderedInts)) shouldNot be (sorted)
      }
      
      def `should use explicitly specified Sortable` {
        intercept[TestFailedException] {
          all(List(orderedInts)) shouldNot be (sorted)
        }
        all(List(orderedInts)) shouldNot be (sorted) (falseSortable)
      }
    }
  }
}
