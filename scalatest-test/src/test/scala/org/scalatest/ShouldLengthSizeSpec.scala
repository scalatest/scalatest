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

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import Integer.MIN_VALUE
import org.scalatest.exceptions.TestFailedException
import org.scalatest.enablers.Length
import org.scalatest.enablers.Size
import Matchers._

class ShouldLengthSizeSpec extends Spec with Checkers with ReturnsNormallyThrowsAssertion {

  object `the 'should have length/size' syntax on an arbitrary object with both length and size methods` {

    class Lengthy(len: Int) {
      def length: Int = len
      def size: Int = len  
      override def toString = "lengthy"
    }
    val obj = new Lengthy(2)

    implicit val lengthOfLengthy =
      new Length[Lengthy] with Size[Lengthy] {
        def lengthOf(o: Lengthy): Long = o.length
        def sizeOf(o: Lengthy): Long = o.size
      }

    def `should do nothing if object length or size matches specified length` {
      obj should have length (2)
      obj should have size (2)
    }

    def `should do nothing if object length or size does not match and used with should not` {
      obj should not { have length (3) }
      obj should not have length (3)
      obj should not { have size (3) }
      obj should not have size (3)
    }

    def `should do nothing when object length or size matches and used in a logical-and expression` {
      obj should { have length (2) and (have length (3 - 1)) }
      obj should (have length (2) and have length (3 - 1))
      obj should { have size (2) and (have size (3 - 1)) }
      obj should (have size (2) and have size (3 - 1))
    }

    def `should do nothing when object length or size matches and used in a logical-or expression` {
      obj should { have length (77) or (have length (3 - 1)) }
      obj should (have length (77) or have length (3 - 1))
      obj should { have size (77) or (have size (3 - 1)) }
      obj should (have size (77) or have size (3 - 1))
    }

    def `should do nothing when object length or size doesn't match and used in a logical-and expression with not` {
      obj should { not { have length (5) } and not { have length (3) }}
      obj should (not have length (5) and not have length (3))
      obj should { not { have size (5) } and not { have size (3) }}
      obj should (not have size (5) and not have size (3))
    }

    def `should do nothing when object length or size doesn't match and used in a logical-or expression with not` {
      obj should { not { have length (2) } or not { have length (3) }}
      obj should (not have length (2) or not have length (3))
      obj should { not { have size (2) } or not { have size (3) }}
      obj should (not have size (2) or not have size (3))
    }

    def `should throw TestFailedException if object length or size does not match specified length` {
      val caught1 = intercept[TestFailedException] {
        obj should have length (3)
      }
      assert(caught1.getMessage === FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 3))
      val caught2 = intercept[TestFailedException] {
        obj should have size (3)
      }
      assert(caught2.getMessage === FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 3))
    }

    def `should throw TestFailedException with normal error message if specified length is negative` {
      val caught1 = intercept[TestFailedException] {
        obj should have length (-2)
      }
      assert(caught1.getMessage === FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, -2))
      val caught2 = intercept[TestFailedException] {
        obj should have size (-2)
      }
      assert(caught2.getMessage === FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, -2))
    }

    def `should throw an assertion error when object length or size doesn't match and used in a logical-and expression` {

      val caught1 = intercept[TestFailedException] {
        obj should { have length (5) and (have length (2 - 1)) }
      }
      assert(caught1.getMessage === FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 5))

      val caught2 = intercept[TestFailedException] {
        obj should ((have length (5)) and (have length (2 - 1)))
      }
      assert(caught2.getMessage === FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 5))

      val caught3 = intercept[TestFailedException] {
        obj should (have length (5) and have length (2 - 1))
      }
      assert(caught3.getMessage === FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 5))

      val caught1b = intercept[TestFailedException] {
        obj should { have size (5) and (have size (2 - 1)) }
      }
      assert(caught1b.getMessage === FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 5))

      val caughtb2 = intercept[TestFailedException] {
        obj should ((have size (5)) and (have size (2 - 1)))
      }
      assert(caughtb2.getMessage === FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 5))

      val caughtb3 = intercept[TestFailedException] {
        obj should (have size (5) and have size (2 - 1))
      }
      assert(caughtb3.getMessage === FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 5))
    }

    def `should throw an assertion error when object length or size doesn't match and used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        obj should { have length (55) or (have length (22)) }
      }
      assert(caught1.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 22))))

      val caught2 = intercept[TestFailedException] {
        obj should ((have length (55)) or (have length (22)))
      }
      assert(caught2.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 22))))

      val caught3 = intercept[TestFailedException] {
        obj should (have length (55) or have length (22))
      }
      assert(caught3.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 22))))

      val caught1b = intercept[TestFailedException] {
        obj should { have size (55) or (have size (22)) }
      }
      assert(caught1b.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 22))))

      val caught2b = intercept[TestFailedException] {
        obj should ((have size (55)) or (have size (22)))
      }
      assert(caught2b.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 22))))

      val caught3b = intercept[TestFailedException] {
        obj should (have size (55) or have size (22))
      }
      assert(caught3b.getMessage === FailureMessages.commaAnd(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 22))))
    }

    def `should throw an assertion error when object length or size matches and used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        obj should { not { have length (3) } and not { have length (2) }}
      }
      assert(caught1.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadLength(UnquotedString("lengthy"), 2))))

      val caught2 = intercept[TestFailedException] {
        obj should { { not have length (3) } and { not have length (2) }}
      }
      assert(caught2.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadLength(UnquotedString("lengthy"), 2))))

      val caught3 = intercept[TestFailedException] {
        obj should (not have length (3) and not have length (2))
      }
      assert(caught3.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadLengthInsteadOfExpectedLength(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadLength(UnquotedString("lengthy"), 2))))

      val caught1b = intercept[TestFailedException] {
        obj should { not { have size (3) } and not { have size (2) }}
      }
      assert(caught1b.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadSize(UnquotedString("lengthy"), 2))))

      val caught2b = intercept[TestFailedException] {
        obj should { { not have size (3) } and { not have size (2) }}
      }
      assert(caught2b.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadSize(UnquotedString("lengthy"), 2))))

      val caught3b = intercept[TestFailedException] {
        obj should (not have size (3) and not have size (2))
      }
      assert(caught3b.getMessage === FailureMessages.commaBut(UnquotedString(FailureMessages.hadSizeInsteadOfExpectedSize(UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages.hadSize(UnquotedString("lengthy"), 2))))
    }

    def `should throw an assertion error when object length or size matches and used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        obj should { not { have length (2) } or not { have length (2) }}
      }
      assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

      val caught2 = intercept[TestFailedException] {
        obj should { { not have length (2) } or { not have length (2) }}
      }
      assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

      val caught3 = intercept[TestFailedException] {
        obj should (not have length (2) or not have length (2))
      }
      assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")

      val caught1b = intercept[TestFailedException] {
        obj should { not { have size (2) } or not { have size (2) }}
      }
      assert(caught1b.getMessage === "lengthy had size 2, and lengthy had size 2")

      val caught2b = intercept[TestFailedException] {
        obj should { { not have size (2) } or { not have size (2) }}
      }
      assert(caught2b.getMessage === "lengthy had size 2, and lengthy had size 2")

      val caught3b = intercept[TestFailedException] {
        obj should (not have size (2) or not have size (2))
      }
      assert(caught3b.getMessage === "lengthy had size 2, and lengthy had size 2")
    }
  }
}
