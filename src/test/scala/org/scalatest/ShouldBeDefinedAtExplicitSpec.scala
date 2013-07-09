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
import org.scalatest.enablers.Definition

class ShouldBeDefinedAtExplicitSpec extends Spec with Matchers {
  
  object `PartialFunction ` {
    
    val left = Array(1, 2, 3)
    
    val left2 = Array(7, 8, 9)
    
    def wasDefinedAt(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " was defined at " + right
    
    def wasNotDefinedAt(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " was not defined at " + right
      
    def equaled(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " equaled " + FailureMessages.decorateToStringValue(right)
      
    def didNotEqual(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " did not equal " + FailureMessages.decorateToStringValue(right)
      
    def wasNotEqualTo(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " was not equal to " + FailureMessages.decorateToStringValue(right)
      
    def wasEqualTo(left: Any, right: Any): String = 
      FailureMessages.decorateToStringValue(left) + " was equal to " + FailureMessages.decorateToStringValue(right)
      
    val arrDef = 
      new Definition[Array[Int]] {
        def isDefinedAt(array: Array[Int], at: Any): Boolean = {
          at != 0
        }
      }
    
    object `should be definedAt` {
      
      def `should do nothing when PartialFunction is defined at the specified value` {
        (left should be definedAt (6)) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is not defined at the specified value` {
        val caught = intercept[TestFailedException] {
          (left should be definedAt (0)) (arrDef)
        }
        assert(caught.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-and expression passed` {
        (left should (be definedAt (6) and be definedAt (8))) (arrDef, arrDef)
        (left should (be definedAt (6) and (be definedAt (8)))) (arrDef)
        (left should (be (definedAt (6)) and be (definedAt (8)))) (arrDef)
        
        (left should (equal (left) and be definedAt (8))) (defaultEquality, arrDef)
        (left should (equal (left) and (be definedAt (8)))) (defaultEquality, arrDef)
        (left should ((equal (left)) and be (definedAt (8)))) (defaultEquality, arrDef)
        
        (left should (be definedAt (6) and equal (left))) (arrDef, defaultEquality)
        (left should (be definedAt (6) and (equal (left)))) (arrDef, defaultEquality)
        (left should (be (definedAt (6)) and (equal (left)))) (arrDef, defaultEquality)
        
        (left should (be (left) and be definedAt (8))) (arrDef)
        (left should (be (left) and (be definedAt (8)))) (arrDef)
        (left should ((be (left)) and be (definedAt (8)))) (arrDef)
        
        (left should (be definedAt (6) and be (left))) (arrDef)
        (left should (be definedAt (6) and (be (left)))) (arrDef)
        (left should (be (definedAt (6)) and (be (left)))) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when first expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (be definedAt (0) and be definedAt (8))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (be definedAt (0) and (be definedAt (8)))) (arrDef)
        }
        assert(caught2.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (be (definedAt (0)) and be (definedAt (8)))) (arrDef)
        }
        assert(caught3.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (equal (left2) and be definedAt (8))) (defaultEquality, arrDef)
        }
        assert(caught4.message === Some(didNotEqual(left, left2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (equal (left2) and (be definedAt (8)))) (defaultEquality, arrDef)
        }
        assert(caught5.message === Some(didNotEqual(left, left2)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should ((equal (left2)) and be (definedAt (8)))) (defaultEquality, arrDef)
        }
        assert(caught6.message === Some(didNotEqual(left, left2)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (be (left2) and be definedAt (8))) (arrDef)
        }
        assert(caught7.message === Some(wasNotEqualTo(left, left2)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          (left should (be (left2) and (be definedAt (8)))) (arrDef)
        }
        assert(caught8.message === Some(wasNotEqualTo(left, left2)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          (left should ((be (left2)) and be (definedAt (8)))) (arrDef)
        }
        assert(caught9.message === Some(wasNotEqualTo(left, left2)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when second expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (be definedAt (8) and be definedAt (0))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotDefinedAt(left, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (be definedAt (8) and (be definedAt (0)))) (arrDef)
        }
        assert(caught2.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotDefinedAt(left, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (be (definedAt (8)) and be (definedAt (0)))) (arrDef)
        }
        assert(caught3.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotDefinedAt(left, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (be definedAt (8) and equal (left2))) (arrDef, defaultEquality)
        }
        assert(caught4.message === Some(wasDefinedAt(left, 8) + ", but " + didNotEqual(left, left2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (be definedAt (8) and (equal (left2)))) (arrDef, defaultEquality)
        }
        assert(caught5.message === Some(wasDefinedAt(left, 8) + ", but " + didNotEqual(left, left2)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should (be (definedAt (8)) and (equal (left2)))) (arrDef, defaultEquality)
        }
        assert(caught6.message === Some(wasDefinedAt(left, 8) + ", but " + didNotEqual(left, left2)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (be definedAt (8) and be (left2))) (arrDef)
        }
        assert(caught7.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotEqualTo(left, left2)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          (left should (be definedAt (8) and (be (left2)))) (arrDef)
        }
        assert(caught8.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotEqualTo(left, left2)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          (left should (be (definedAt (8)) and (be (left2)))) (arrDef)
        }
        assert(caught9.message === Some(wasDefinedAt(left, 8) + ", but " + wasNotEqualTo(left, left2)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (be definedAt (0) and be definedAt (0))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (be definedAt (0) and (be definedAt (0)))) (arrDef)
        }
        assert(caught2.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (be (definedAt (0)) and be (definedAt (0)))) (arrDef)
        }
        assert(caught3.message === Some(wasNotDefinedAt(left, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-or expression passed` {
        (left should (be definedAt (6) or be definedAt (8))) (arrDef, arrDef)
        (left should (be definedAt (6) or (be definedAt (8)))) (arrDef)
        (left should (be (definedAt (6)) or be (definedAt (8)))) (arrDef)
        
        (left should (equal (left) or be definedAt (8))) (defaultEquality, arrDef)
        (left should (equal (left) or (be definedAt (8)))) (defaultEquality, arrDef)
        (left should ((equal (left)) or be (definedAt (8)))) (defaultEquality, arrDef)
        
        (left should (be definedAt (6) or equal (left))) (arrDef, defaultEquality)
        (left should (be definedAt (6) or (equal (left)))) (arrDef, defaultEquality)
        (left should (be (definedAt (6)) or (equal (left)))) (arrDef, defaultEquality)
        
        (left should (be (left) or be definedAt (8))) (arrDef)
        (left should (be (left) or (be definedAt (8)))) (arrDef)
        (left should ((be (left)) or be (definedAt (8)))) (arrDef)
        
        (left should (be definedAt (6) or be (left))) (arrDef)
        (left should (be definedAt (6) or (be (left)))) (arrDef)
        (left should (be (definedAt (6)) or (be (left)))) (arrDef)
      }
      
      def `should do nothing when first expression in logical-or expression failed` {
        (left should (be definedAt (0) or be definedAt (8))) (arrDef, arrDef)
        (left should (be definedAt (0) or (be definedAt (8)))) (arrDef)
        (left should (be (definedAt (0)) or be (definedAt (8)))) (arrDef)
        
        (left should (equal (left2) or be definedAt (8))) (defaultEquality, arrDef)
        (left should (equal (left2) or (be definedAt (8)))) (defaultEquality, arrDef)
        (left should ((equal (left2)) or be (definedAt (8)))) (defaultEquality, arrDef)
        
        (left should (be definedAt (0) or equal (left))) (arrDef, defaultEquality)
        (left should (be definedAt (0) or (equal (left)))) (arrDef, defaultEquality)
        (left should (be (definedAt (0)) or (equal (left)))) (arrDef, defaultEquality)
        
        (left should (be (left2) or be definedAt (8))) (arrDef)
        (left should (be (left2) or (be definedAt (8)))) (arrDef)
        (left should ((be (left2)) or be (definedAt (8)))) (arrDef)
        
        (left should (be definedAt (0) or be (left))) (arrDef)
        (left should (be definedAt (0) or (be (left)))) (arrDef)
        (left should (be (definedAt (0)) or (be (left)))) (arrDef)
      }
      
      def `should do nothing when second expressions in logical-or expression failed` {
        (left should (be definedAt (6) or be definedAt (0))) (arrDef, arrDef)
        (left should (be definedAt (6) or (be definedAt (0)))) (arrDef)
        (left should (be (definedAt (6)) or be (definedAt (0)))) (arrDef)
        
        (left should (equal (left) or be definedAt (0))) (defaultEquality, arrDef)
        (left should (equal (left) or (be definedAt (0)))) (defaultEquality, arrDef)
        (left should ((equal (left)) or be (definedAt (0)))) (defaultEquality, arrDef)
        
        (left should (be definedAt (6) or equal (left2))) (arrDef, defaultEquality)
        (left should (be definedAt (6) or (equal (left2)))) (arrDef, defaultEquality)
        (left should (be (definedAt (6)) or (equal (left2)))) (arrDef, defaultEquality)
        
        (left should (be (left) or be definedAt (0))) (arrDef)
        (left should (be (left) or (be definedAt (0)))) (arrDef)
        (left should ((be (left)) or be (definedAt (0)))) (arrDef)
        
        (left should (be definedAt (6) or be (left2))) (arrDef)
        (left should (be definedAt (6) or (be (left2)))) (arrDef)
        (left should (be (definedAt (6)) or (be (left2)))) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-or expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (be definedAt (0) or be definedAt (0)))  (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasNotDefinedAt(left, 0) + ", and " + wasNotDefinedAt(left, 0)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (be definedAt (0) or (be definedAt (0)))) (arrDef)
        }
        assert(caught2.message === Some(wasNotDefinedAt(left, 0) + ", and " + wasNotDefinedAt(left, 0)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (be (definedAt (0)) or be (definedAt (0)))) (arrDef)
        }
        assert(caught3.message === Some(wasNotDefinedAt(left, 0) + ", and " + wasNotDefinedAt(left, 0)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (be definedAt (0) or equal (left2))) (arrDef, defaultEquality)
        }
        assert(caught4.message === Some(wasNotDefinedAt(left, 0) + ", and " + didNotEqual(left, left2)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (equal (left2) or (be definedAt (0)))) (defaultEquality, arrDef)
        }
        assert(caught5.message === Some(didNotEqual(left, left2) + ", and " + wasNotDefinedAt(left, 0)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should (be definedAt (0) or be (left2))) (arrDef)
        }
        assert(caught6.message === Some(wasNotDefinedAt(left, 0) + ", and " + wasNotEqualTo(left, left2)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (be (left2) or (be definedAt (0)))) (arrDef)
        }
        assert(caught7.message === Some(wasNotEqualTo(left, left2) + ", and " + wasNotDefinedAt(left, 0)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    object `should not be definedAt` {
      
      def `should do nothing when PartialFunction is not defined at the specified value` {
        (left should not be definedAt (0)) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value` {
        val caught = intercept[TestFailedException] {
          (left should not be definedAt (8)) (arrDef)
        }
        assert(caught.message === Some(wasDefinedAt(left, 8)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-and expression passed` {
        (left should (not be definedAt (0) and not be definedAt (0))) (arrDef, arrDef)
        (left should (not be definedAt (0) and (not be definedAt (0)))) (arrDef)
        (left should (not be (definedAt (0)) and not be (definedAt (0)))) (arrDef, arrDef)
        
        (left should (not equal (left2) and not be definedAt (0))) (defaultEquality, arrDef)
        (left should (not equal (left2) and (not be definedAt (0)))) (defaultEquality, arrDef)
        (left should ((not equal (left2)) and not be (definedAt (0)))) (defaultEquality, arrDef)
        
        (left should (not be definedAt (0) and not equal (left2))) (arrDef, defaultEquality)
        (left should (not be definedAt (0) and (not equal (left2)))) (arrDef, defaultEquality)
        (left should (not be (definedAt (0)) and (not equal (left2)))) (arrDef, defaultEquality)
        
        (left should (not be (left2) and not be definedAt (0))) (arrDef)
        (left should (not be (left2) and (not be definedAt (0)))) (arrDef)
        (left should ((not be (left2)) and not be (definedAt (0)))) (arrDef)
        
        (left should (not be definedAt (0) and not be (left2))) (arrDef)
        (left should (not be definedAt (0) and (not be (left2)))) (arrDef)
        (left should (not be (definedAt (0)) and (not be (left2)))) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when first expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (not be definedAt (8) and not be definedAt (0))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasDefinedAt(left, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (not be definedAt (8) and (not be definedAt (0)))) (arrDef)
        }
        assert(caught2.message === Some(wasDefinedAt(left, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (not be (definedAt (8)) and not be (definedAt (0)))) (arrDef, arrDef)
        }
        assert(caught3.message === Some(wasDefinedAt(left, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (not equal (left) and not be definedAt (0))) (defaultEquality, arrDef)
        }
        assert(caught4.message === Some(equaled(left, left)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (not equal (left) and (not be definedAt (8)))) (defaultEquality, arrDef)
        }
        assert(caught5.message === Some(equaled(left, left)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should ((not equal (left)) and not be (definedAt (8)))) (defaultEquality, arrDef)
        }
        assert(caught6.message === Some(equaled(left, left)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (not be (left) and not be definedAt (0))) (arrDef)
        }
        assert(caught7.message === Some(wasEqualTo(left, left)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          (left should (not be (left) and (not be definedAt (8)))) (arrDef)
        }
        assert(caught8.message === Some(wasEqualTo(left, left)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          (left should ((not be (left)) and not be (definedAt (8)))) (arrDef)
        }
        assert(caught9.message === Some(wasEqualTo(left, left)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when second expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and not be definedAt (8))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasDefinedAt(left, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and (not be definedAt (8)))) (arrDef)
        }
        assert(caught2.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasDefinedAt(left, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (not be (definedAt (0)) and not be (definedAt (8)))) (arrDef, arrDef)
        }
        assert(caught3.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasDefinedAt(left, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and not equal (left))) (arrDef, defaultEquality)
        }
        assert(caught4.message === Some(wasNotDefinedAt(left, 0) + ", but " + equaled(left, left)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and (not equal (left)))) (arrDef, defaultEquality)
        }
        assert(caught5.message === Some(wasNotDefinedAt(left, 0) + ", but " + equaled(left, left)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should (not be (definedAt (0)) and (not equal (left)))) (arrDef, defaultEquality)
        }
        assert(caught6.message === Some(wasNotDefinedAt(left, 0) + ", but " + equaled(left, left)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and not be (left))) (arrDef)
        }
        assert(caught7.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasEqualTo(left, left)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          (left should (not be definedAt (0) and (not be (left)))) (arrDef)
        }
        assert(caught8.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasEqualTo(left, left)))
        assert(caught8.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          (left should (not be (definedAt (0)) and (not be (left)))) (arrDef)
        }
        assert(caught9.message === Some(wasNotDefinedAt(left, 0) + ", but " + wasEqualTo(left, left)))
        assert(caught9.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-and expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (not be definedAt (8) and not be definedAt (8))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasDefinedAt(left, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (not be definedAt (8) and (not be definedAt (8)))) (arrDef)
        }
        assert(caught2.message === Some(wasDefinedAt(left, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (not be (definedAt (8)) and not be (definedAt (8)))) (arrDef, arrDef)
        }
        assert(caught3.message === Some(wasDefinedAt(left, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should do nothing when both expressions in logical-or expression passed` {
        (left should (not be definedAt (0) or not be definedAt (0))) (arrDef, arrDef)
        (left should (not be definedAt (0) or (not be definedAt (0)))) (arrDef)
        (left should (not be (definedAt (0)) or not be (definedAt (0)))) (arrDef, arrDef)
        
        (left should (not equal (left2) or not be definedAt (0))) (defaultEquality, arrDef)
        (left should (not equal (left2) or (not be definedAt (0)))) (defaultEquality, arrDef)
        (left should ((not equal (left2)) or not be (definedAt (0)))) (defaultEquality, arrDef)
        
        (left should (not be definedAt (0) or not equal (left2))) (arrDef, defaultEquality)
        (left should (not be definedAt (0) or (not equal (left2)))) (arrDef, defaultEquality)
        (left should (not be (definedAt (0)) or (not equal (left2)))) (arrDef, defaultEquality)
        
        (left should (not be (left2) or not be definedAt (0))) (arrDef)
        (left should (not be (left2) or (not be definedAt (0)))) (arrDef)
        (left should ((not be (left2)) or not be (definedAt (0)))) (arrDef)
        
        (left should (not be definedAt (0) or not be (left2))) (arrDef)
        (left should (not be definedAt (0) or (not be (left2)))) (arrDef)
        (left should (not be (definedAt (0)) or (not be (left2)))) (arrDef)
      }
      
      def `should do nothing when first expression in logical-or expression failed` {
        (left should (not be definedAt (8) or not be definedAt (0))) (arrDef, arrDef)
        (left should (not be definedAt (8) or (not be definedAt (0)))) (arrDef)
        (left should (not be (definedAt (8)) or not be (definedAt (0)))) (arrDef, arrDef)
        
        (left should (not equal (left) or not be definedAt (0))) (defaultEquality, arrDef)
        (left should (not equal (left) or (not be definedAt (0)))) (defaultEquality, arrDef)
        (left should ((not equal (left)) or not be (definedAt (0)))) (defaultEquality, arrDef)
        
        (left should (not be definedAt (8) or not equal (left2))) (arrDef, defaultEquality)
        (left should (not be definedAt (8) or (not equal (left2)))) (arrDef, defaultEquality)
        (left should (not be (definedAt (8)) or (not equal (left2)))) (arrDef, defaultEquality)
        
        (left should (not be (left) or not be definedAt (0))) (arrDef)
        (left should (not be (left) or (not be definedAt (0)))) (arrDef)
        (left should ((not be (left)) or not be (definedAt (0)))) (arrDef)
        
        (left should (not be definedAt (8) or not be (left2))) (arrDef)
        (left should (not be definedAt (8) or (not be (left2)))) (arrDef)
        (left should (not be (definedAt (8)) or (not be (left2)))) (arrDef)
      }
      
      def `should do nothing when second expressions in logical-or expression failed` {
        (left should (not be definedAt (0) or not be definedAt (8))) (arrDef, arrDef)
        (left should (not be definedAt (0) or (not be definedAt (8)))) (arrDef)
        (left should (not be (definedAt (0)) or not be (definedAt (8)))) (arrDef, arrDef)
        
        (left should (not equal (left2) or not be definedAt (8))) (defaultEquality, arrDef)
        (left should (not equal (left2) or (not be definedAt (8)))) (defaultEquality, arrDef)
        (left should ((not equal (left2)) or not be (definedAt (8)))) (defaultEquality, arrDef)
        
        (left should (not be definedAt (0) or not equal (left))) (arrDef, defaultEquality)
        (left should (not be definedAt (0) or (not equal (left)))) (arrDef, defaultEquality)
        (left should (not be (definedAt (0)) or (not equal (left)))) (arrDef, defaultEquality)
        
        (left should (not be (left2) or not be definedAt (8))) (arrDef)
        (left should (not be (left2) or (not be definedAt (8)))) (arrDef)
        (left should ((not be (left2)) or not be (definedAt (8)))) (arrDef)
        
        (left should (not be definedAt (0) or not be (left))) (arrDef)
        (left should (not be definedAt (0) or (not be (left)))) (arrDef)
        (left should (not be (definedAt (0)) or (not be (left)))) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when both expression in logical-or expression failed` {
        val caught1 = intercept[TestFailedException] {
          (left should (not be definedAt (8) or not be definedAt (8))) (arrDef, arrDef)
        }
        assert(caught1.message === Some(wasDefinedAt(left, 8) + ", and " + wasDefinedAt(left, 8)))
        assert(caught1.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          (left should (not be definedAt (8) or (not be definedAt (8)))) (arrDef)
        }
        assert(caught2.message === Some(wasDefinedAt(left, 8) + ", and " + wasDefinedAt(left, 8)))
        assert(caught2.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          (left should (not be (definedAt (8)) or not be (definedAt (8)))) (arrDef, arrDef)
        }
        assert(caught3.message === Some(wasDefinedAt(left, 8) + ", and " + wasDefinedAt(left, 8)))
        assert(caught3.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          (left should (not be definedAt (8) or not equal (left))) (arrDef, defaultEquality)
        }
        assert(caught4.message === Some(wasDefinedAt(left, 8) + ", and " + equaled(left, left)))
        assert(caught4.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          (left should (not equal (left) or (not be definedAt (8)))) (defaultEquality, arrDef)
        }
        assert(caught5.message === Some(equaled(left, left) + ", and " + wasDefinedAt(left, 8)))
        assert(caught5.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          (left should (not be definedAt (8) or not be (left))) (arrDef)
        }
        assert(caught6.message === Some(wasDefinedAt(left, 8) + ", and " + wasEqualTo(left, left)))
        assert(caught6.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          (left should (not be (left) or (not be definedAt (8)))) (arrDef)
        }
        assert(caught7.message === Some(wasEqualTo(left, left) + ", and " + wasDefinedAt(left, 8)))
        assert(caught7.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    object `shouldNot be definedAt` {
      
      def `should do nothing when PartialFunction is not defined at the specified value` {
        (left shouldNot be definedAt (0)) (arrDef)
      }
      
      def `should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value` {
        val caught = intercept[TestFailedException] {
          (left shouldNot be definedAt (8)) (arrDef)
        }
        assert(caught.message === Some(wasDefinedAt(left, 8)))
        assert(caught.failedCodeFileName === Some("ShouldBeDefinedAtExplicitSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
  
}