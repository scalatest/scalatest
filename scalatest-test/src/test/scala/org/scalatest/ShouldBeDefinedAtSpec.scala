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

class ShouldBeDefinedAtSpec extends FunSpec {
  
  describe("PartialFunction ") {
    
    val fraction = new PartialFunction[Int, Int] {
      def apply(d: Int) = 42 / d
      def isDefinedAt(d: Int) = d != 0
    }
    
    val fraction2 = new PartialFunction[Int, Int] {
      def apply(d: Int) = 42 / d
      def isDefinedAt(d: Int) = d != 0
    }
    
    def wasDefinedAt(left: Any, right: Any): String = 
      left + " was defined at " + right
    
    def wasNotDefinedAt(left: Any, right: Any): String = 
      left + " was not defined at " + right
      
    def equaled(left: Any, right: Any): String = 
      left + " equaled " + right
      
    def didNotEqual(left: Any, right: Any): String = 
      left + " did not equal " + right
    
    describe("should be definedAt") {
      
      it("should do nothing when PartialFunction is defined at the specified value") {
        fraction should be definedAt (6)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is not defined at the specified value") {
        val caught = intercept[TestFailedException] {
          fraction should be definedAt (0)
        }
        assert(caught.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-and expression passed") {
        fraction should (be definedAt (6) and be definedAt (8))
        fraction should (be definedAt (6) and (be definedAt (8)))
        fraction should (be (definedAt (6)) and be (definedAt (8)))
        
        fraction should (equal (fraction) and be definedAt (8))
        fraction should (equal (fraction) and (be definedAt (8)))
        fraction should ((equal (fraction)) and be (definedAt (8)))
        
        fraction should (be definedAt (6) and equal (fraction))
        fraction should (be definedAt (6) and (equal (fraction)))
        fraction should (be (definedAt (6)) and (equal (fraction)))
      }
      
      it("should throw TestFailedException with correct stack depth when first expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (be definedAt (0) and be definedAt (8))
        }
        assert(caught1.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (be definedAt (0) and (be definedAt (8)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (be (definedAt (0)) and be (definedAt (8)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (equal (fraction2) and be definedAt (8))
        }
        assert(caught4.message === Some(didNotEqual(fraction, fraction2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (equal (fraction2) and (be definedAt (8)))
        }
        assert(caught5.message === Some(didNotEqual(fraction, fraction2)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          fraction should ((equal (fraction2)) and be (definedAt (8)))
        }
        assert(caught6.message === Some(didNotEqual(fraction, fraction2)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when second expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (be definedAt (8) and be definedAt (0))
        }
        assert(caught1.message === Some(wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (be definedAt (8) and (be definedAt (0)))
        }
        assert(caught2.message === Some(wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (be (definedAt (8)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(wasDefinedAt(fraction, 8) + ", but " + wasNotDefinedAt(fraction, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (be definedAt (8) and equal (fraction2))
        }
        assert(caught4.message === Some(wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (be definedAt (8) and (equal (fraction2)))
        }
        assert(caught5.message === Some(wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          fraction should (be (definedAt (8)) and (equal (fraction2)))
        }
        assert(caught6.message === Some(wasDefinedAt(fraction, 8) + ", but " + didNotEqual(fraction, fraction2)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (be definedAt (0) and be definedAt (0))
        }
        assert(caught1.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (be definedAt (0) and (be definedAt (0)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (be (definedAt (0)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(fraction, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-or expression passed") {
        fraction should (be definedAt (6) or be definedAt (8))
        fraction should (be definedAt (6) or (be definedAt (8)))
        fraction should (be (definedAt (6)) or be (definedAt (8)))
        
        fraction should (equal (fraction) or be definedAt (8))
        fraction should (equal (fraction) or (be definedAt (8)))
        fraction should ((equal (fraction)) or be (definedAt (8)))
        
        fraction should (be definedAt (6) or equal (fraction))
        fraction should (be definedAt (6) or (equal (fraction)))
        fraction should (be (definedAt (6)) or (equal (fraction)))
      }
      
      it("should do nothing when first expression in logical-or expression failed") {
        fraction should (be definedAt (0) or be definedAt (8))
        fraction should (be definedAt (0) or (be definedAt (8)))
        fraction should (be (definedAt (0)) or be (definedAt (8)))
        
        fraction should (equal (fraction2) or be definedAt (8))
        fraction should (equal (fraction2) or (be definedAt (8)))
        fraction should ((equal (fraction2)) or be (definedAt (8)))
        
        fraction should (be definedAt (0) or equal (fraction))
        fraction should (be definedAt (0) or (equal (fraction)))
        fraction should (be (definedAt (0)) or (equal (fraction)))
      }
      
      it("should do nothing when second expressions in logical-or expression failed") {
        fraction should (be definedAt (6) or be definedAt (0))
        fraction should (be definedAt (6) or (be definedAt (0)))
        fraction should (be (definedAt (6)) or be (definedAt (0)))
        
        fraction should (equal (fraction) or be definedAt (0))
        fraction should (equal (fraction) or (be definedAt (0)))
        fraction should ((equal (fraction)) or be (definedAt (0)))
        
        fraction should (be definedAt (6) or equal (fraction2))
        fraction should (be definedAt (6) or (equal (fraction2)))
        fraction should (be (definedAt (6)) or (equal (fraction2)))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-or expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (be definedAt (0) or be definedAt (0))
        }
        assert(caught1.message === Some(wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (be definedAt (0) or (be definedAt (0)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (be (definedAt (0)) or be (definedAt (0)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(fraction, 0) + ", and " + wasNotDefinedAt(fraction, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (be definedAt (0) or equal (fraction2))
        }
        assert(caught4.message === Some(wasNotDefinedAt(fraction, 0) + ", and " + didNotEqual(fraction, fraction2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (equal (fraction2) or (be definedAt (0)))
        }
        assert(caught5.message === Some(didNotEqual(fraction, fraction2) + ", and " + wasNotDefinedAt(fraction, 0)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("should not be definedAt") {
      
      it("should do nothing when PartialFunction is not defined at the specified value") {
        fraction should not be definedAt (0)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value") {
        val caught = intercept[TestFailedException] {
          fraction should not be definedAt (8)
        }
        assert(caught.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-and expression passed") {
        fraction should (not be definedAt (0) and not be definedAt (0))
        fraction should (not be definedAt (0) and (not be definedAt (0)))
        fraction should (not be (definedAt (0)) and not be (definedAt (0)))
        
        fraction should (not equal (fraction2) and not be definedAt (0))
        fraction should (not equal (fraction2) and (not be definedAt (0)))
        fraction should ((not equal (fraction2)) and not be (definedAt (0)))
        
        fraction should (not be definedAt (0) and not equal (fraction2))
        fraction should (not be definedAt (0) and (not equal (fraction2)))
        fraction should (not be (definedAt (0)) and (not equal (fraction2)))
      }
      
      it("should throw TestFailedException with correct stack depth when first expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) and not be definedAt (0))
        }
        assert(caught1.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) and (not be definedAt (0)))
        }
        assert(caught2.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (not be (definedAt (8)) and not be (definedAt (0)))
        }
        assert(caught3.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (not equal (fraction) and not be definedAt (0))
        }
        assert(caught4.message === Some(equaled(fraction, fraction)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (not equal (fraction) and (not be definedAt (8)))
        }
        assert(caught5.message === Some(equaled(fraction, fraction)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          fraction should ((not equal (fraction)) and not be (definedAt (8)))
        }
        assert(caught6.message === Some(equaled(fraction, fraction)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when second expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (not be definedAt (0) and not be definedAt (8))
        }
        assert(caught1.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (not be definedAt (0) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (not be (definedAt (0)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + wasDefinedAt(fraction, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (not be definedAt (0) and not equal (fraction))
        }
        assert(caught4.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (not be definedAt (0) and (not equal (fraction)))
        }
        assert(caught5.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          fraction should (not be (definedAt (0)) and (not equal (fraction)))
        }
        assert(caught6.message === Some(wasNotDefinedAt(fraction, 0) + ", but " + equaled(fraction, fraction)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) and not be definedAt (8))
        }
        assert(caught1.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (not be (definedAt (8)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-or expression passed") {
        fraction should (not be definedAt (0) or not be definedAt (0))
        fraction should (not be definedAt (0) or (not be definedAt (0)))
        fraction should (not be (definedAt (0)) or not be (definedAt (0)))
        
        fraction should (not equal (fraction2) or not be definedAt (0))
        fraction should (not equal (fraction2) or (not be definedAt (0)))
        fraction should ((not equal (fraction2)) or not be (definedAt (0)))
        
        fraction should (not be definedAt (0) or not equal (fraction2))
        fraction should (not be definedAt (0) or (not equal (fraction2)))
        fraction should (not be (definedAt (0)) or (not equal (fraction2)))
      }
      
      it("should do nothing when first expression in logical-or expression failed") {
        fraction should (not be definedAt (8) or not be definedAt (0))
        fraction should (not be definedAt (8) or (not be definedAt (0)))
        fraction should (not be (definedAt (8)) or not be (definedAt (0)))
        
        fraction should (not equal (fraction) or not be definedAt (0))
        fraction should (not equal (fraction) or (not be definedAt (0)))
        fraction should ((not equal (fraction)) or not be (definedAt (0)))
        
        fraction should (not be definedAt (8) or not equal (fraction2))
        fraction should (not be definedAt (8) or (not equal (fraction2)))
        fraction should (not be (definedAt (8)) or (not equal (fraction2)))
      }
      
      it("should do nothing when second expressions in logical-or expression failed") {
        fraction should (not be definedAt (0) or not be definedAt (8))
        fraction should (not be definedAt (0) or (not be definedAt (8)))
        fraction should (not be (definedAt (0)) or not be (definedAt (8)))
        
        fraction should (not equal (fraction2) or not be definedAt (8))
        fraction should (not equal (fraction2) or (not be definedAt (8)))
        fraction should ((not equal (fraction2)) or not be (definedAt (8)))
        
        fraction should (not be definedAt (0) or not equal (fraction))
        fraction should (not be definedAt (0) or (not equal (fraction)))
        fraction should (not be (definedAt (0)) or (not equal (fraction)))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-or expression failed") {
        val caught1 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) or not be definedAt (8))
        }
        assert(caught1.message === Some(wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) or (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          fraction should (not be (definedAt (8)) or not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasDefinedAt(fraction, 8) + ", and " + wasDefinedAt(fraction, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          fraction should (not be definedAt (8) or not equal (fraction))
        }
        assert(caught4.message === Some(wasDefinedAt(fraction, 8) + ", and " + equaled(fraction, fraction)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          fraction should (not equal (fraction) or (not be definedAt (8)))
        }
        assert(caught5.message === Some(equaled(fraction, fraction) + ", and " + wasDefinedAt(fraction, 8)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("shouldNot be definedAt") {
      
      it("should do nothing when PartialFunction is not defined at the specified value") {
        fraction shouldNot be definedAt (0)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value") {
        val caught = intercept[TestFailedException] {
          fraction shouldNot be definedAt (8)
        }
        assert(caught.message === Some(wasDefinedAt(fraction, 8)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
  
}
