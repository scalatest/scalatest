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

import prop.Checkers
import Integer.MIN_VALUE
import org.scalacheck._
import Arbitrary._
import Prop._
import Matchers._
import exceptions.TestFailedException

class ShouldStructuralSizeSpec extends FunSpec with Checkers with ReturnsNormallyThrowsAssertion {
  
  def hadSizeInsteadOfExpectedSize(left: Any, leftSize: Long, expectedSize: Long): String = 
    FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
    
  def hadSize(left: Any, expectedSize: Int): String = 
    FailureMessages("hadSize", left, expectedSize)
    
  def commaAnd(left: String, right: String): String = 
    FailureMessages("commaAnd", UnquotedString(left), UnquotedString(right))
  
  def commaBut(left: String, right: String): String = 
    FailureMessages("commaBut", UnquotedString(left), UnquotedString(right))
  
  describe("The 'have size (Int)' syntax ") {
    
    describe("on an arbitrary object that has an empty-paren Int size method") {
  
      class Sizey(len: Int) {
        def size(): Int = len
        override def toString = "sizey"
      }
      val obj = new Sizey(2)
  
      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }
  
      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }
  
      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }
  
      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }
  
      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }
  
      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }
  
      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }
  
      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }
  
      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }
  
      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }
  
      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }
  
      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Int size method") {

      class Sizey(len: Int) {
        def size: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Int size field") {

      class Sizey(len: Int) {
        val size: Int = len // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    
    describe("on an arbitrary object that has an empty-paren Int getSize method") {

      class Sizey(len: Int) {
        def getSize(): Int = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Int getSize method") {

      class Sizey(len: Int) {
        def getSize: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has an Int getSize field") {

      class Sizey(len: Int) {
        val getSize: Int = len // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
  }
  
  describe("The 'have length (Long)' syntax ") {
    describe("on an arbitrary object that has an empty-paren Long size method") {

      class Sizey(len: Long) {
        def size(): Long = len
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        obj should not { have size (3L) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should { have size (2L) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should ((have size (2L)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
        obj should (have size (2L) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (2L)) }
        obj should { have size (77L) or (have size (2)) }
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Long size method") {

      class Sizey(len: Long) {
        def size: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        obj should not { have size (3L) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Long size field") {

      class Sizey(len: Long) {
        val size: Long = len // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.size, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.size, 55), hadSizeInsteadOfExpectedSize(obj, obj.size, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.size, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has an empty-paren Long getSize method") {

      class Sizey(len: Long) {
        def getSize(): Long = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        obj should not { have size (3) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Long getSize method") {

      class Sizey(len: Long) {
        def getSize: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        obj should not { have size (3L) }
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Long getSize field") {

      class Sizey(len: Long) {
        val getSize: Long = len // The only difference between the previous is the structure of this member
        override def toString = "sizey"
      }
      val obj = new Sizey(2)

      it("should do nothing if object size matches specified size") {
        obj should have size (2)
        obj should have size (2L)
        check((len: Int) => returnsNormally(new Sizey(len) should have size (len)))
        check((len: Long) => returnsNormally(new Sizey(len) should have size (len)))
      }

      it("should do nothing if object size does not match and used with should not") {
        obj should not { have size (3) }
        obj should not have size (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Sizey(len) should not { have size (wrongLen) }))
      }

      it("should do nothing when object size matches and used in a logical-and expression") {
        obj should { have size (2) and (have size (3 - 1)) }
        obj should ((have size (2)) and (have size (3 - 1)))
        obj should (have size (2) and have size (3 - 1))
      }

      it("should do nothing when object size matches and used in a logical-or expression") {
        obj should { have size (77) or (have size (3 - 1)) }
        obj should ((have size (77)) or (have size (3 - 1)))
        obj should (have size (77) or have size (3 - 1))
      }

      it("should do nothing when object size doesn't match and used in a logical-and expression with not") {
        obj should { not { have size (5) } and not { have size (3) }}
        obj should ((not have size (5)) and (not have size (3)))
        obj should (not have size (5) and not have size (3))
      }

      it("should do nothing when object size doesn't match and used in a logical-or expression with not") {
        obj should { not { have size (2) } or not { have size (3) }}
        obj should ((not have size (2)) or (not have size (3)))
        obj should (not have size (2) or not have size (3))
      }

      it("should throw TestFailedException if object size does not match specified size") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified size is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, -2))
        check((len: Int) => throwsTestFailedException(new Sizey(len) should have size (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caught2.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caught3.getMessage === hadSizeInsteadOfExpectedSize(obj, obj.getSize, 5))
      }

      it("should throw an assertion error when object size doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3.getMessage === commaAnd(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 55), hadSizeInsteadOfExpectedSize(obj, obj.getSize, 22)))
      }

      it("should throw an assertion error when object size matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (3)) and (not have size (2)))
        }
        assert(caught2.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3.getMessage === commaBut(hadSizeInsteadOfExpectedSize(obj, obj.getSize, 3), hadSize(obj, 2)))
      }

      it("should throw an assertion error when object size matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should ((not have size (2)) or (not have size (2)))
        }
        assert(caught2.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3.getMessage === commaAnd(hadSize(obj, 2), hadSize(obj, 2)))
      }
    }
  }
}
