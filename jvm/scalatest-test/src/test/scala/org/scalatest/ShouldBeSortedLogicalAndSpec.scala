/*
 * Copyright 2001-2024 Artima, Inc.
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
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeSortedLogicalAndSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  //ADDITIONAL//
  
  def wasEqualTo(left: Any, right: Any): String = 
    decorateToStringValue(prettifier, left) + " was equal to " + decorateToStringValue(prettifier, right)
    
  def wasNotEqualTo(left: Any, right: Any): String = 
    decorateToStringValue(prettifier, left) + " was not equal to " + decorateToStringValue(prettifier, right)
    
  def equaled(left: Any, right: Any): String = 
    decorateToStringValue(prettifier, left) + " equaled " + decorateToStringValue(prettifier, right)
    
  def didNotEqual(left: Any, right: Any): String = 
    decorateToStringValue(prettifier, left) + " did not equal " + decorateToStringValue(prettifier, right)
  
  def wasNotSorted(left: Any): String = 
    decorateToStringValue(prettifier, left) + " was not sorted"
    
  def wasSorted(left: Any): String = 
    decorateToStringValue(prettifier, left) + " was sorted"
    
  def allInspectionFailed(idx: Int, message: String, lineNumber:Int, left: Any) = 
    "'all' inspection failed, because: \n" + 
    "  at index " + idx + ", " + message + " (ShouldBeSortedLogicalAndSpec.scala:" + lineNumber + ") \n" + 
    "in " + decorateToStringValue(prettifier, left)
    
  case class Student(name: String, scores: Int)
  implicit val studentOrdering: Ordering[Student] = new Ordering[Student] {
    def compare(a: Student, b: Student) = a.scores compare b.scores
  }
  
  val orderedInts = List(1, 2, 3)
  val outOfOrderInts = List(3, 2, 1)
  
  val orderedStudents = List(Student("Student 1", 80), Student("Student 2", 88), Student("Student 3", 90))
  val outOfOrderStudents = List(Student("Student 3", 90), Student("Student 2", 88), Student("Student 1", 80))
  
  val orderedString = "123"
  val outOfOrderString = "321"
  
  val trueSortable = 
    new Sortable[List[Int]] {
      def isSorted(o: List[Int]) = true
    }
  
  val falseSortable = 
    new Sortable[List[Int]] {
      def isSorted(o: List[Int]) = false
    }
  
  describe("Sorted matcher") {
    
    describe("when work with 'xs should be (sorted)'") {
      
      it("should do nothing when xs is sorted") {
        orderedInts should (be (sorted) and be (sorted))
        orderedInts should (be (sorted) and be (orderedInts))
        orderedInts should (be (orderedInts) and be (sorted))
        
        orderedStudents should (be (sorted) and be (sorted))
        orderedStudents should (be (sorted) and equal (orderedStudents))
        orderedStudents should (equal (orderedStudents) and be (sorted))

        orderedString should (be (sorted) and be (sorted))
        orderedString should (be (sorted) and be (orderedString))
        orderedString should (be (orderedString) and be (sorted))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
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

        val caught5 = intercept[TestFailedException] {
          outOfOrderString should (be (outOfOrderString) and be (sorted))
        }
        assert(caught5.message === Some(wasEqualTo(outOfOrderString, outOfOrderString) + ", but " + wasNotSorted(outOfOrderString)))
        assert(caught5.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          outOfOrderString should (be (sorted) and be (outOfOrderString))
        }
        assert(caught6.message === Some(wasNotSorted(outOfOrderString)))
        assert(caught6.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should use implicit Sortable when available") {
        intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) and be (outOfOrderInts))
        }

        {
          implicit val imp = trueSortable
          outOfOrderInts should (be (sorted) and be (outOfOrderInts))
        }
      }
      
      it("should use explicitly specified Sortable") {
        intercept[TestFailedException] {
          outOfOrderInts should (be (sorted) and equal (outOfOrderInts))
        }
        (outOfOrderInts should (be (sorted) and equal (outOfOrderInts))) (trueSortable, defaultEquality)
      }
    }
    
    describe("when work with 'xs should not be sorted'") {
      
      it("should do nothing when xs is not sorted") {
        outOfOrderInts should (not be sorted and not be outOfOrderStudents)
        outOfOrderInts should (not be outOfOrderStudents and not be sorted)
        
        outOfOrderStudents should (not be sorted and not equal outOfOrderInts)
        outOfOrderStudents should (not equal outOfOrderInts and not be sorted)

        outOfOrderString should (not be sorted and not be outOfOrderStudents)
        outOfOrderString should (not be outOfOrderStudents and not be sorted)
      }

      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
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

        val caught5 = intercept[TestFailedException] {
          orderedString should (not be outOfOrderStudents and not be sorted)
        }
        assert(caught5.message === Some(wasNotEqualTo(orderedString, outOfOrderStudents) + ", but " + wasSorted(orderedString)))
        assert(caught5.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          orderedString should (not be sorted and not be outOfOrderStudents)
        }
        assert(caught6.message === Some(wasSorted(orderedString)))
        assert(caught6.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should use implicit Sortable when available") {
        intercept[TestFailedException] {
          orderedInts should (not be (sorted) and not be (outOfOrderInts))
        }

        {
          implicit val imp = falseSortable
          orderedInts should (not be (sorted) and not be (outOfOrderInts))
        }
      }
      
      it("should use explicitly specified Sortable") {
        intercept[TestFailedException] {
          orderedInts should (not be (sorted) and not equal (outOfOrderInts))
        }
        (orderedInts should (not be (sorted) and not equal (outOfOrderInts))) (falseSortable, defaultEquality)
      }
    }
    
    describe("when work with 'all(xs) should be (sorted)'") {
      
      it("should do nothing when xs is sorted") {
        all(List(orderedInts)) should (be (sorted) and be (sorted))
        all(List(orderedInts)) should (be (sorted) and be (orderedInts))
        all(List(orderedInts)) should (be (orderedInts) and be (sorted))
        
        all(List(orderedStudents)) should (be (sorted) and be (sorted))
        all(List(orderedStudents)) should (be (sorted) and equal (orderedStudents))
        all(List(orderedStudents)) should (equal (orderedStudents) and be (sorted))

        all(List(orderedString)) should (be (sorted) and be (sorted))
        all(List(orderedString)) should (be (sorted) and be (orderedString))
        all(List(orderedString)) should (be (orderedString) and be (sorted))
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
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

        val left5 = List(outOfOrderString)
        val caught5 = intercept[TestFailedException] {
          all(left5) should (be (outOfOrderString) and be (sorted))
        }
        assert(caught5.message === Some(allInspectionFailed(0, wasEqualTo(outOfOrderString, outOfOrderString) + ", but " + wasNotSorted(outOfOrderString), thisLineNumber - 2, left5)))
        assert(caught5.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left6 = List(outOfOrderString)
        val caught6 = intercept[TestFailedException] {
          all(left6) should (be (sorted) and be (outOfOrderString))
        }
        assert(caught6.message === Some(allInspectionFailed(0, wasNotSorted(outOfOrderString), thisLineNumber - 2, left6)))
        assert(caught6.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should use implicit Sortable when available") {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should (be (sorted) and be (outOfOrderInts))
        }

        {
          implicit val imp = trueSortable
          all(List(outOfOrderInts)) should (be (sorted) and be (outOfOrderInts))
        }
      }
      
      it("should use explicitly specified Sortable") {
        intercept[TestFailedException] {
          all(List(outOfOrderInts)) should (be (sorted) and equal (outOfOrderInts))
        }
        (all(List(outOfOrderInts)) should (be (sorted) and equal (outOfOrderInts))) (trueSortable, defaultEquality)
      }
    }
    
    describe("when work with 'all(xs) should not be sorted'") {
      it("should do nothing when xs is not sorted") {
        all(List(outOfOrderInts)) should (not be sorted and not be outOfOrderStudents)
        all(List(outOfOrderInts)) should (not be outOfOrderStudents and not be sorted)
        
        all(List(outOfOrderStudents)) should (not be sorted and not equal outOfOrderInts)
        all(List(outOfOrderStudents)) should (not equal outOfOrderInts and not be sorted)

        all(List(outOfOrderString)) should (not be sorted and not be outOfOrderStudents)
        all(List(outOfOrderString)) should (not be outOfOrderStudents and not be sorted)
      }
      
      it("should throw TestFailedException with correct stack depth when xs is not sorted") {
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

        val left5 = List(orderedString)
        val caught5 = intercept[TestFailedException] {
          all(left5) should (not be outOfOrderStudents and not be sorted)
        }
        assert(caught5.message === Some(allInspectionFailed(0, wasNotEqualTo(orderedString, outOfOrderStudents) + ", but " + wasSorted(orderedString), thisLineNumber - 2, left5)))
        assert(caught5.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val left6 = List(orderedString)
        val caught6 = intercept[TestFailedException] {
          all(left6) should (not be sorted and not be outOfOrderStudents)
        }
        assert(caught6.message === Some(allInspectionFailed(0, wasSorted(orderedString), thisLineNumber - 2, left6)))
        assert(caught6.failedCodeFileName === Some("ShouldBeSortedLogicalAndSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should use implicit Sortable when available") {
        intercept[TestFailedException] {
          all(List(orderedInts)) should (not be (sorted) and not be (outOfOrderInts))
        }

        {
          implicit val imp = falseSortable
          all(List(orderedInts)) should (not be (sorted) and not be (outOfOrderInts))
        }
      }
      
      it("should use explicitly specified Sortable") {
        intercept[TestFailedException] {
          all(List(orderedInts)) should (not be (sorted) and not equal (outOfOrderInts))
        }
        (all(List(orderedInts)) should (not be (sorted) and not equal (outOfOrderInts))) (falseSortable, defaultEquality)
      }
    }
    
    // shouldBe and shouldNot does not support logical expression, and they never will!
  }
}
