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

class ShouldStructuralLengthSpec extends FunSpec with Checkers with ReturnsNormallyThrowsAssertion {
  
  def hadLengthInsteadOfExpectedLength(left: Any, leftLength: Long, expectedLength: Long): String = 
    FailureMessages.hadLengthInsteadOfExpectedLength(left, leftLength, expectedLength)
    
  def hadLength(left: Any, expectedLength: Int): String = 
    FailureMessages.hadLength(left, expectedLength)
    
  def commaAnd(left: String, right: String): String = 
    FailureMessages.commaAnd(UnquotedString(left), UnquotedString(right))
  
  def commaBut(left: String, right: String): String = 
    FailureMessages.commaBut(UnquotedString(left), UnquotedString(right))
    
  describe("The 'have length (Int)' syntax ") {
    
    describe("on an arbitrary object that has an empty-paren Int length method") {
      
      class Lengthy(len: Int) {
        def length(): Int = len
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)
      
      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }
  
      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }
  
      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }
  
      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }
  
      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }
  
      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }
  
      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }
  
      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }
  
      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }
  
      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }
  
      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }
  
      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
      
    }
    
    describe("on an arbitrary object that has a parameterless Int length method") {

      class Lengthy(len: Int) {
        def length: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Int length field") {

      class Lengthy(len: Int) {
        val length: Int = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has an empty-paren Int getLength method") {

      class Lengthy(len: Int) {
        def getLength(): Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Int getLength method") {

      class Lengthy(len: Int) {
        def getLength: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has an Int getLength field") {

      class Lengthy(len: Int) {
        val getLength: Int = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
  }
  
  describe("The 'have length (Long)' syntax ") {
    
    describe("on an arbitrary object that has an empty-paren Long length method") {

      class Lengthy(len: Long) {
        def length(): Long = len
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should { have length (2L) and (have length (3 - 1)) }
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (2L)) }
        obj should { have length (77L) or (have length (2)) }
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Long length method") {

      class Lengthy(len: Long) {
        def length: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Long length field") {

      class Lengthy(len: Long) {
        val length: Long = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.length, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.length, 55), hadLengthInsteadOfExpectedLength(obj, obj.length, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.length, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has an empty-paren Long getLength method") {

      class Lengthy(len: Long) {
        def getLength(): Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a parameterless Long getLength method") {

      class Lengthy(len: Long) {
        def getLength: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
    
    describe("on an arbitrary object that has a Long getLength field") {

      class Lengthy(len: Long) {
        val getLength: Long = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      it("should do nothing if object length matches specified length") {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      it("should do nothing if object length does not match and used with should not") {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      it("should do nothing when object length matches and used in a logical-and expression") {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      it("should do nothing when object length matches and used in a logical-or expression") {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      it("should do nothing when object length doesn't match and used in a logical-and expression with not") {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      it("should do nothing when object length doesn't match and used in a logical-or expression with not") {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      it("should throw TestFailedException if object length does not match specified length") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      it("should throw TestFailedException with normal error message if specified length is negative") {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === hadLengthInsteadOfExpectedLength(obj, obj.getLength, 5))
      }

      it("should throw an assertion error when object length doesn't match and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === commaAnd(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 55), hadLengthInsteadOfExpectedLength(obj, obj.getLength, 22)))
      }

      it("should throw an assertion error when object length matches and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === commaBut(hadLengthInsteadOfExpectedLength(obj, obj.getLength, 3), hadLength(obj, 2)))
      }

      it("should throw an assertion error when object length matches and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === commaAnd(hadLength(obj, 2), hadLength(obj, 2)))
      }
    }
  }
}
