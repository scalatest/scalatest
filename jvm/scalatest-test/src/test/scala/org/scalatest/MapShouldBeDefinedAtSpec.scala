/*
 * Copyright 2001-2025 Artima, Inc.
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
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class MapShouldBeDefinedAtSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  def wasDefinedAt(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " was defined at " + decorateToStringValue(prettifier, right)
    
  def wasNotDefinedAt(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " was not defined at " + decorateToStringValue(prettifier, right)
      
  def equaled(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " equaled " + decorateToStringValue(prettifier, right)
      
  def didNotEqual(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " did not equal " + decorateToStringValue(prettifier, right)
  
  describe("PartialFunction ") {
    
    val map = Map(6 -> "six", 8 -> "eight")
    
    val map2 = Map(6 -> "enam", 8 -> "lapan")
    
    describe("should be definedAt") {
      
      it("should do nothing when PartialFunction is defined at the specified value") {
        map should be definedAt (6)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is not defined at the specified value") {
        val caught = intercept[TestFailedException] {
          map should be definedAt (0)
        }
        assert(caught.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-and expression passed") {
        map should (be definedAt (6) and be definedAt (8))
        map should (be definedAt (6) and (be definedAt (8)))
        map should (be (definedAt (6)) and be (definedAt (8)))
        
        map should (equal (map) and be definedAt (8))
        map should (equal (map) and (be definedAt (8)))
        map should ((equal (map)) and be (definedAt (8)))
        
        map should (be definedAt (6) and equal (map))
        map should (be definedAt (6) and (equal (map)))
        map should (be (definedAt (6)) and (equal (map)))
      }
      
      it("should throw TestFailedException with correct stack depth when first expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (be definedAt (0) and be definedAt (8))
        }
        assert(caught1.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (be definedAt (0) and (be definedAt (8)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (be (definedAt (0)) and be (definedAt (8)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (equal (map2) and be definedAt (8))
        }
        assert(caught4.message === Some(didNotEqual(map, map2)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (equal (map2) and (be definedAt (8)))
        }
        assert(caught5.message === Some(didNotEqual(map, map2)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          map should ((equal (map2)) and be (definedAt (8)))
        }
        assert(caught6.message === Some(didNotEqual(map, map2)))
        assert(caught6.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when second expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (be definedAt (8) and be definedAt (0))
        }
        assert(caught1.message === Some(wasDefinedAt(map, 8) + ", but " + wasNotDefinedAt(map, 0)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (be definedAt (8) and (be definedAt (0)))
        }
        assert(caught2.message === Some(wasDefinedAt(map, 8) + ", but " + wasNotDefinedAt(map, 0)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (be (definedAt (8)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(wasDefinedAt(map, 8) + ", but " + wasNotDefinedAt(map, 0)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (be definedAt (8) and equal (map2))
        }
        assert(caught4.message === Some(wasDefinedAt(map, 8) + ", but " + didNotEqual(map, map2)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (be definedAt (8) and (equal (map2)))
        }
        assert(caught5.message === Some(wasDefinedAt(map, 8) + ", but " + didNotEqual(map, map2)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          map should (be (definedAt (8)) and (equal (map2)))
        }
        assert(caught6.message === Some(wasDefinedAt(map, 8) + ", but " + didNotEqual(map, map2)))
        assert(caught6.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (be definedAt (0) and be definedAt (0))
        }
        assert(caught1.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (be definedAt (0) and (be definedAt (0)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (be (definedAt (0)) and be (definedAt (0)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(map, 0)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-or expression passed") {
        map should (be definedAt (6) or be definedAt (8))
        map should (be definedAt (6) or (be definedAt (8)))
        map should (be (definedAt (6)) or be (definedAt (8)))
        
        map should (equal (map) or be definedAt (8))
        map should (equal (map) or (be definedAt (8)))
        map should ((equal (map)) or be (definedAt (8)))
        
        map should (be definedAt (6) or equal (map))
        map should (be definedAt (6) or (equal (map)))
        map should (be (definedAt (6)) or (equal (map)))
      }
      
      it("should do nothing when first expression in logical-or expression failed") {
        map should (be definedAt (0) or be definedAt (8))
        map should (be definedAt (0) or (be definedAt (8)))
        map should (be (definedAt (0)) or be (definedAt (8)))
        
        map should (equal (map2) or be definedAt (8))
        map should (equal (map2) or (be definedAt (8)))
        map should ((equal (map2)) or be (definedAt (8)))
        
        map should (be definedAt (0) or equal (map))
        map should (be definedAt (0) or (equal (map)))
        map should (be (definedAt (0)) or (equal (map)))
      }
      
      it("should do nothing when second expressions in logical-or expression failed") {
        map should (be definedAt (6) or be definedAt (0))
        map should (be definedAt (6) or (be definedAt (0)))
        map should (be (definedAt (6)) or be (definedAt (0)))
        
        map should (equal (map) or be definedAt (0))
        map should (equal (map) or (be definedAt (0)))
        map should ((equal (map)) or be (definedAt (0)))
        
        map should (be definedAt (6) or equal (map2))
        map should (be definedAt (6) or (equal (map2)))
        map should (be (definedAt (6)) or (equal (map2)))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-or expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (be definedAt (0) or be definedAt (0))
        }
        assert(caught1.message === Some(wasNotDefinedAt(map, 0) + ", and " + wasNotDefinedAt(map, 0)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (be definedAt (0) or (be definedAt (0)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(map, 0) + ", and " + wasNotDefinedAt(map, 0)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (be (definedAt (0)) or be (definedAt (0)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(map, 0) + ", and " + wasNotDefinedAt(map, 0)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (be definedAt (0) or equal (map2))
        }
        assert(caught4.message === Some(wasNotDefinedAt(map, 0) + ", and " + didNotEqual(map, map2)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (equal (map2) or (be definedAt (0)))
        }
        assert(caught5.message === Some(didNotEqual(map, map2) + ", and " + wasNotDefinedAt(map, 0)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
    
    describe("should not be definedAt") {
      
      it("should do nothing when PartialFunction is not defined at the specified value") {
        map should not be definedAt (0)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value") {
        val caught = intercept[TestFailedException] {
          map should not be definedAt (8)
        }
        assert(caught.message === Some(wasDefinedAt(map, 8)))
        assert(caught.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-and expression passed") {
        map should (not be definedAt (0) and not be definedAt (0))
        map should (not be definedAt (0) and (not be definedAt (0)))
        map should (not be (definedAt (0)) and not be (definedAt (0)))
        
        map should (not equal (map2) and not be definedAt (0))
        map should (not equal (map2) and (not be definedAt (0)))
        map should ((not equal (map2)) and not be (definedAt (0)))
        
        map should (not be definedAt (0) and not equal (map2))
        map should (not be definedAt (0) and (not equal (map2)))
        map should (not be (definedAt (0)) and (not equal (map2)))
      }
      
      it("should throw TestFailedException with correct stack depth when first expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (not be definedAt (8) and not be definedAt (0))
        }
        assert(caught1.message === Some(wasDefinedAt(map, 8)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (not be definedAt (8) and (not be definedAt (0)))
        }
        assert(caught2.message === Some(wasDefinedAt(map, 8)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (not be (definedAt (8)) and not be (definedAt (0)))
        }
        assert(caught3.message === Some(wasDefinedAt(map, 8)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (not equal (map) and not be definedAt (0))
        }
        assert(caught4.message === Some(equaled(map, map)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (not equal (map) and (not be definedAt (8)))
        }
        assert(caught5.message === Some(equaled(map, map)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          map should ((not equal (map)) and not be (definedAt (8)))
        }
        assert(caught6.message === Some(equaled(map, map)))
        assert(caught6.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when second expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (not be definedAt (0) and not be definedAt (8))
        }
        assert(caught1.message === Some(wasNotDefinedAt(map, 0) + ", but " + wasDefinedAt(map, 8)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (not be definedAt (0) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasNotDefinedAt(map, 0) + ", but " + wasDefinedAt(map, 8)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (not be (definedAt (0)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasNotDefinedAt(map, 0) + ", but " + wasDefinedAt(map, 8)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (not be definedAt (0) and not equal (map))
        }
        assert(caught4.message === Some(wasNotDefinedAt(map, 0) + ", but " + equaled(map, map)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (not be definedAt (0) and (not equal (map)))
        }
        assert(caught5.message === Some(wasNotDefinedAt(map, 0) + ", but " + equaled(map, map)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          map should (not be (definedAt (0)) and (not equal (map)))
        }
        assert(caught6.message === Some(wasNotDefinedAt(map, 0) + ", but " + equaled(map, map)))
        assert(caught6.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-and expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (not be definedAt (8) and not be definedAt (8))
        }
        assert(caught1.message === Some(wasDefinedAt(map, 8)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (not be definedAt (8) and (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasDefinedAt(map, 8)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (not be (definedAt (8)) and not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasDefinedAt(map, 8)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should do nothing when both expressions in logical-or expression passed") {
        map should (not be definedAt (0) or not be definedAt (0))
        map should (not be definedAt (0) or (not be definedAt (0)))
        map should (not be (definedAt (0)) or not be (definedAt (0)))
        
        map should (not equal (map2) or not be definedAt (0))
        map should (not equal (map2) or (not be definedAt (0)))
        map should ((not equal (map2)) or not be (definedAt (0)))
        
        map should (not be definedAt (0) or not equal (map2))
        map should (not be definedAt (0) or (not equal (map2)))
        map should (not be (definedAt (0)) or (not equal (map2)))
      }
      
      it("should do nothing when first expression in logical-or expression failed") {
        map should (not be definedAt (8) or not be definedAt (0))
        map should (not be definedAt (8) or (not be definedAt (0)))
        map should (not be (definedAt (8)) or not be (definedAt (0)))
        
        map should (not equal (map) or not be definedAt (0))
        map should (not equal (map) or (not be definedAt (0)))
        map should ((not equal (map)) or not be (definedAt (0)))
        
        map should (not be definedAt (8) or not equal (map2))
        map should (not be definedAt (8) or (not equal (map2)))
        map should (not be (definedAt (8)) or (not equal (map2)))
      }
      
      it("should do nothing when second expressions in logical-or expression failed") {
        map should (not be definedAt (0) or not be definedAt (8))
        map should (not be definedAt (0) or (not be definedAt (8)))
        map should (not be (definedAt (0)) or not be (definedAt (8)))
        
        map should (not equal (map2) or not be definedAt (8))
        map should (not equal (map2) or (not be definedAt (8)))
        map should ((not equal (map2)) or not be (definedAt (8)))
        
        map should (not be definedAt (0) or not equal (map))
        map should (not be definedAt (0) or (not equal (map)))
        map should (not be (definedAt (0)) or (not equal (map)))
      }
      
      it("should throw TestFailedException with correct stack depth when both expression in logical-or expression failed") {
        val caught1 = intercept[TestFailedException] {
          map should (not be definedAt (8) or not be definedAt (8))
        }
        assert(caught1.message === Some(wasDefinedAt(map, 8) + ", and " + wasDefinedAt(map, 8)))
        assert(caught1.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          map should (not be definedAt (8) or (not be definedAt (8)))
        }
        assert(caught2.message === Some(wasDefinedAt(map, 8) + ", and " + wasDefinedAt(map, 8)))
        assert(caught2.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          map should (not be (definedAt (8)) or not be (definedAt (8)))
        }
        assert(caught3.message === Some(wasDefinedAt(map, 8) + ", and " + wasDefinedAt(map, 8)))
        assert(caught3.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          map should (not be definedAt (8) or not equal (map))
        }
        assert(caught4.message === Some(wasDefinedAt(map, 8) + ", and " + equaled(map, map)))
        assert(caught4.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          map should (not equal (map) or (not be definedAt (8)))
        }
        assert(caught5.message === Some(equaled(map, map) + ", and " + wasDefinedAt(map, 8)))
        assert(caught5.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
    describe("shouldNot be definedAt") {
      
      it("should do nothing when PartialFunction is not defined at the specified value") {
        map shouldNot be definedAt (0)
      }
      
      it("should throw TestFailedException with correct stack depth when PartialFunction is defined at the specified value") {
        val caught = intercept[TestFailedException] {
          map shouldNot be definedAt (8)
        }
        assert(caught.message === Some(wasDefinedAt(map, 8)))
        assert(caught.failedCodeFileName === Some("MapShouldBeDefinedAtSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
  
}
