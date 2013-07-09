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

class ShouldBeDefinedAtForAllSpec extends Spec with Matchers {
  
  def wasDefinedAt(left: Any, right: Any): String = 
    left + " was defined at " + right
    
  def wasNotDefinedAt(left: Any, right: Any): String = 
    left + " was not defined at " + right
      
  def equaled(left: Any, right: Any): String = 
    left + " equaled " + right
      
  def didNotEqual(left: Any, right: Any): String = 
    left + " did not equal " + right
    
  def wasNotEqualTo(left: Any, right: Any): String = 
      left + " was not equal to " + right
      
    def wasEqualTo(left: Any, right: Any): String = 
      left + " was equal to " + right
  
  def errorMessage(index: Int, message: String, lineNumber: Int, left: Any): String = 
    "'all' inspection failed, because: \n" +
    "  at index " + index + ", " + message + " (ShouldBeDefinedAtForAllSpec.scala:" + lineNumber + ") \n" +
    "in " + left
  
  object `PartialFunction ` {
    
    val fraction = new PartialFunction[Int, Int] {
      def apply(d: Int) = 42 / d
      def isDefinedAt(d: Int) = d != 0
    }
    
    val fraction2 = new PartialFunction[Int, Int] {
      def apply(d: Int) = 42 / d
      def isDefinedAt(d: Int) = d != 0
    }
    
    val list = List(fraction)
    
    object `all(xs) should be definedAt` {
      
      def `should do nothing when PartialFunction is defined at the specified value` {
        all(List(fraction)) should be definedAt (6)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is not defined at the specified value` {
        val caught = intercept[TestFailedException] {
          all(list) should be definedAt (0)
        }
        assert(caught.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-and expression passed` {
        all(list) should (be definedAt (6) and be definedAt (8))
        all(list) should (be definedAt (6) and (be definedAt (8)))
        all(list) should (be (definedAt (6)) and be (definedAt (8)))
        
        all(list) should (equal (fraction) and be definedAt (8))
        all(list) should (equal (fraction) and (be definedAt (8)))
        all(list) should ((equal (fraction)) and be (definedAt (8)))
        
        all(list) should (be definedAt (6) and equal (fraction))
        all(list) should (be definedAt (6) and (equal (fraction)))
        all(list) should (be (definedAt (6)) and (equal (fraction)))
        
        all(list) should (be (fraction) and be definedAt (8))
        all(list) should (be (fraction) and (be definedAt (8)))
        all(list) should ((be (fraction)) and be (definedAt (8)))
        
        all(list) should (be definedAt (6) and be (fraction))
        all(list) should (be definedAt (6) and (be (fraction)))
        all(list) should (be (definedAt (6)) and (be (fraction)))
      }
      
      def `should throw TestFailedException with correct stack depth when first expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) and be definedAt (8))
        }
        assert(caught1.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) and (be definedAt (8)))
        }
        assert(caught2.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (be (definedAt (0)) and be (definedAt (8)))
        }
        assert(caught3.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (equal (fraction2) and be definedAt (8))
        }
        assert(caught4.message === Some(errorMessage(0, didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (equal (fraction2) and (be definedAt (8)))
        }
        assert(caught5.message === Some(errorMessage(0, didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          all(list) should ((equal (fraction2)) and be (definedAt (8)))
        }
        assert(caught6.message === Some(errorMessage(0, didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          all(list) should (be (fraction2) and be definedAt (8))
        }
        assert(caught7.message === Some(errorMessage(0, wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          all(list) should (be (fraction2) and (be definedAt (8)))
        }
        assert(caught8.message === Some(errorMessage(0, wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          all(list) should ((be (fraction2)) and be (definedAt (8)))
        }
        assert(caught9.message === Some(errorMessage(0, wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when second expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and be definedAt (0))
        }
        assert(caught1.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and (be definedAt (0)))
        }
        assert(caught2.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (be (definedAt (8)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and equal (fraction2))
        }
        assert(caught4.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and (equal (fraction2)))
        }
        assert(caught5.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          all(list) should (be (definedAt (8)) and (equal (fraction2)))
        }
        assert(caught6.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and be (fraction2))
        }
        assert(caught7.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          all(list) should (be definedAt (8) and (be (fraction2)))
        }
        assert(caught8.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          all(list) should (be (definedAt (8)) and (be (fraction2)))
        }
        assert(caught9.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", but " + wasNotEqualTo(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) and be definedAt (0))
        }
        assert(caught1.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) and (be definedAt (0)))
        }
        assert(caught2.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (be (definedAt (0)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-or expression passed` {
        all(list) should (be definedAt (6) or be definedAt (8))
        all(list) should (be definedAt (6) or (be definedAt (8)))
        all(list) should (be (definedAt (6)) or be (definedAt (8)))
        
        all(list) should (equal (fraction) or be definedAt (8))
        all(list) should (equal (fraction) or (be definedAt (8)))
        all(list) should ((equal (fraction)) or be (definedAt (8)))
        
        all(list) should (be definedAt (6) or equal (fraction))
        all(list) should (be definedAt (6) or (equal (fraction)))
        all(list) should (be (definedAt (6)) or (equal (fraction)))
        
        all(list) should (be (fraction) or be definedAt (8))
        all(list) should (be (fraction) or (be definedAt (8)))
        all(list) should ((be (fraction)) or be (definedAt (8)))
        
        all(list) should (be definedAt (6) or be (fraction))
        all(list) should (be definedAt (6) or (be (fraction)))
        all(list) should (be (definedAt (6)) or (be (fraction)))
      }
      
      def `should do nothing when first expression in logical-or expression failed` {
        all(list) should (be definedAt (0) or be definedAt (8))
        all(list) should (be definedAt (0) or (be definedAt (8)))
        all(list) should (be (definedAt (0)) or be (definedAt (8)))
        
        all(list) should (equal (fraction2) or be definedAt (8))
        all(list) should (equal (fraction2) or (be definedAt (8)))
        all(list) should ((equal (fraction2)) or be (definedAt (8)))
        
        all(list) should (be definedAt (0) or equal (fraction))
        all(list) should (be definedAt (0) or (equal (fraction)))
        all(list) should (be (definedAt (0)) or (equal (fraction)))
        
        all(list) should (be (fraction2) or be definedAt (8))
        all(list) should (be (fraction2) or (be definedAt (8)))
        all(list) should ((be (fraction2)) or be (definedAt (8)))
        
        all(list) should (be definedAt (0) or be (fraction))
        all(list) should (be definedAt (0) or (be (fraction)))
        all(list) should (be (definedAt (0)) or (be (fraction)))
      }
      
      def `should do nothing when second expressions in logical-or expression failed` {
        all(list) should (be definedAt (6) or be definedAt (0))
        all(list) should (be definedAt (6) or (be definedAt (0)))
        all(list) should (be (definedAt (6)) or be (definedAt (0)))
        
        all(list) should (equal (fraction) or be definedAt (0))
        all(list) should (equal (fraction) or (be definedAt (0)))
        all(list) should ((equal (fraction)) or be (definedAt (0)))
        
        all(list) should (be definedAt (6) or equal (fraction2))
        all(list) should (be definedAt (6) or (equal (fraction2)))
        all(list) should (be (definedAt (6)) or (equal (fraction2)))
        
        all(list) should (be (fraction) or be definedAt (0))
        all(list) should (be (fraction) or (be definedAt (0)))
        all(list) should ((be (fraction)) or be (definedAt (0)))
        
        all(list) should (be definedAt (6) or be (fraction2))
        all(list) should (be definedAt (6) or (be (fraction2)))
        all(list) should (be (definedAt (6)) or (be (fraction2)))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-or expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) or be definedAt (0))
        }
        assert(caught1.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) or (be definedAt (0)))
        }
        assert(caught2.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (be (definedAt (0)) or be (definedAt (0)))
        }
        assert(caught3.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (be definedAt (0) or equal (fraction2))
        }
        assert(caught4.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", and " + didNotEqual(fraction, fraction2), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (equal (fraction2) or (be definedAt (0)))
        }
        assert(caught5.message === Some(errorMessage(0, didNotEqual(fraction, fraction2) + ", and " + wasNotDefinedAt(fraction, 0), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `all(xs) should not be definedAt` {
      
      def `should do nothing when PartialFunction is not defined at the specified value` {
        all(list) should not be definedAt (0)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value` {
        val caught = intercept[TestFailedException] {
          all(list) should not be definedAt (8)
        }
        assert(caught.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-and expression passed` {
        all(list) should (not be definedAt (0) and not be definedAt (0))
        all(list) should (not be definedAt (0) and (not be definedAt (0)))
        all(list) should (not be (definedAt (0)) and not be (definedAt (0)))
        
        all(list) should (not equal (fraction2) and not be definedAt (0))
        all(list) should (not equal (fraction2) and (not be definedAt (0)))
        all(list) should ((not equal (fraction2)) and not be (definedAt (0)))
        
        all(list) should (not be definedAt (0) and not equal (fraction2))
        all(list) should (not be definedAt (0) and (not equal (fraction2)))
        all(list) should (not be (definedAt (0)) and (not equal (fraction2)))
        
        all(list) should (not be (fraction2) and not be definedAt (0))
        all(list) should (not be (fraction2) and (not be definedAt (0)))
        all(list) should ((not be (fraction2)) and not be (definedAt (0)))
        
        all(list) should (not be definedAt (0) and not be (fraction2))
        all(list) should (not be definedAt (0) and (not be (fraction2)))
        all(list) should (not be (definedAt (0)) and (not be (fraction2)))
      }
      
      def `should throw TestFailedException with correct stack depth when first expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) and not be definedAt (0))
        }
        assert(caught1.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) and (not be definedAt (0)))
        }
        assert(caught2.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (8)) and not be (definedAt (0)))
        }
        assert(caught3.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (not equal (fraction) and not be definedAt (0))
        }
        assert(caught4.message === Some(errorMessage(0, equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (not equal (fraction) and (not be definedAt (8)))
        }
        assert(caught5.message === Some(errorMessage(0, equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          all(list) should ((not equal (fraction)) and not be (definedAt (8)))
        }
        assert(caught6.message === Some(errorMessage(0, equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          all(list) should (not be (fraction) and not be definedAt (0))
        }
        assert(caught7.message === Some(errorMessage(0, wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          all(list) should (not be (fraction) and (not be definedAt (8)))
        }
        assert(caught8.message === Some(errorMessage(0, wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          all(list) should ((not be (fraction)) and not be (definedAt (8)))
        }
        assert(caught9.message === Some(errorMessage(0, wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when second expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and not be definedAt (8))
        }
        assert(caught1.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (0)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and not equal (fraction))
        }
        assert(caught4.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and (not equal (fraction)))
        }
        assert(caught5.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (0)) and (not equal (fraction)))
        }
        assert(caught6.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and not be (fraction))
        }
        assert(caught7.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          all(list) should (not be definedAt (0) and (not be (fraction)))
        }
        assert(caught8.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (0)) and (not be (fraction)))
        }
        assert(caught9.message === Some(errorMessage(0, wasNotDefinedAt(fraction, 0) + ", but " + wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) and not be definedAt (8))
        }
        assert(caught1.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (8)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-or expression passed` {
        all(list) should (not be definedAt (0) or not be definedAt (0))
        all(list) should (not be definedAt (0) or (not be definedAt (0)))
        all(list) should (not be (definedAt (0)) or not be (definedAt (0)))
        
        all(list) should (not equal (fraction2) or not be definedAt (0))
        all(list) should (not equal (fraction2) or (not be definedAt (0)))
        all(list) should ((not equal (fraction2)) or not be (definedAt (0)))
        
        all(list) should (not be definedAt (0) or not equal (fraction2))
        all(list) should (not be definedAt (0) or (not equal (fraction2)))
        all(list) should (not be (definedAt (0)) or (not equal (fraction2)))
        
        all(list) should (not be (fraction2) or not be definedAt (0))
        all(list) should (not be (fraction2) or (not be definedAt (0)))
        all(list) should ((not be (fraction2)) or not be (definedAt (0)))
        
        all(list) should (not be definedAt (0) or not be (fraction2))
        all(list) should (not be definedAt (0) or (not be (fraction2)))
        all(list) should (not be (definedAt (0)) or (not be (fraction2)))
      }
      
      def `should do nothing when first expression in logical-or expression failed` {
        all(list) should (not be definedAt (8) or not be definedAt (0))
        all(list) should (not be definedAt (8) or (not be definedAt (0)))
        all(list) should (not be (definedAt (8)) or not be (definedAt (0)))
        
        all(list) should (not equal (fraction) or not be definedAt (0))
        all(list) should (not equal (fraction) or (not be definedAt (0)))
        all(list) should ((not equal (fraction)) or not be (definedAt (0)))
        
        all(list) should (not be definedAt (8) or not equal (fraction2))
        all(list) should (not be definedAt (8) or (not equal (fraction2)))
        all(list) should (not be (definedAt (8)) or (not equal (fraction2)))
        
        all(list) should (not be (fraction) or not be definedAt (0))
        all(list) should (not be (fraction) or (not be definedAt (0)))
        all(list) should ((not be (fraction)) or not be (definedAt (0)))
        
        all(list) should (not be definedAt (8) or not be (fraction2))
        all(list) should (not be definedAt (8) or (not be (fraction2)))
        all(list) should (not be (definedAt (8)) or (not be (fraction2)))
      }
      
      def `should do nothing when second expressions in logical-or expression failed` {
        all(list) should (not be definedAt (0) or not be definedAt (8))
        all(list) should (not be definedAt (0) or (not be definedAt (8)))
        all(list) should (not be (definedAt (0)) or not be (definedAt (8)))
        
        all(list) should (not equal (fraction2) or not be definedAt (8))
        all(list) should (not equal (fraction2) or (not be definedAt (8)))
        all(list) should ((not equal (fraction2)) or not be (definedAt (8)))
        
        all(list) should (not be definedAt (0) or not equal (fraction))
        all(list) should (not be definedAt (0) or (not equal (fraction)))
        all(list) should (not be (definedAt (0)) or (not equal (fraction)))
        
        all(list) should (not be (fraction2) or not be definedAt (8))
        all(list) should (not be (fraction2) or (not be definedAt (8)))
        all(list) should ((not be (fraction2)) or not be (definedAt (8)))
        
        all(list) should (not be definedAt (0) or not be (fraction))
        all(list) should (not be definedAt (0) or (not be (fraction)))
        all(list) should (not be (definedAt (0)) or (not be (fraction)))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-or expression failed` {
        val caught1 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) or not be definedAt (8))
        }
        assert(caught1.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) or (not be definedAt (8)))
        }
        assert(caught2.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          all(list) should (not be (definedAt (8)) or not be (definedAt (8)))
        }
        assert(caught3.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) or not equal (fraction))
        }
        assert(caught4.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", and " + equaled(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          all(list) should (not equal (fraction) or (not be definedAt (8)))
        }
        assert(caught5.message === Some(errorMessage(0, equaled(fraction, fraction) + ", and " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          all(list) should (not be definedAt (8) or not be (fraction))
        }
        assert(caught6.message === Some(errorMessage(0, wasDefinedAt(fraction, 8) + ", and " + wasEqualTo(fraction, fraction), thisLineNumber - 2, list)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          all(list) should (not be (fraction) or (not be definedAt (8)))
        }
        assert(caught7.message === Some(errorMessage(0, wasEqualTo(fraction, fraction) + ", and " + wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `all(xs) shouldNot be definedAt` {
      
      def `should do nothing when PartialFunction is not defined at the specified value` {
        all(list) shouldNot be definedAt (0)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value` {
        val caught = intercept[TestFailedException] {
          all(list) shouldNot be definedAt (8)
        }
        assert(caught.message === Some(errorMessage(0, wasDefinedAt(fraction, 8), thisLineNumber - 2, list)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtForAllSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
  
}