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

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldLogicalMatcherExprSpec extends AnyFunSpec with ReturnsNormallyThrowsAssertion {

  sealed abstract class Clown {
    def hasBigRedNose = true
  }

  class CountingClown extends Clown {
    val count = new AtomicInteger(0)
    override def hasBigRedNose: Boolean = {
      count.incrementAndGet()
      super.hasBigRedNose
    }
  }

  def mock[T]: Clown = new CountingClown

  def verify(clown: Clown, expectedCount: Int): Unit = {
    clown match {
      case countingClown: CountingClown => assert(countingClown.count.get() == expectedCount)
      case other => fail("Expected CountingClown, but we got: " + other)
    }
  }

  describe("Matcher expressions to the right of and") {
    describe("(A plain-old matcher)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and { mockClown.hasBigRedNose; have length (2) })
        }
 
        verify(mockClown, 1)
      }
    }

    describe("(have length N syntax)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and have length {mockClown.hasBigRedNose; 2})
        }

        verify(mockClown, 1)

        intercept[TestFailedException] {
          "hi" should (have length (1) and {mockClown.hasBigRedNose; have length 2})
        }

        verify(mockClown, 2)
      }
    }

    describe("(not have length N syntax)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and not have length {mockClown.hasBigRedNose; 1})
        }

        verify(mockClown, 1)

        intercept[TestFailedException] {
          "hi" should (have length (1) and not {mockClown.hasBigRedNose; have length (1)})
        }

        verify(mockClown, 2)

        intercept[TestFailedException] {
          "hi" should (have length (1) and {mockClown.hasBigRedNose; not have length (1)})
        }

        verify(mockClown, 3)
      }
    }

    describe("(have size N syntax)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and have size {mockClown.hasBigRedNose; 2})
        }

        verify(mockClown, 1)

        intercept[TestFailedException] {
          "hi" should (have size (1) and {mockClown.hasBigRedNose; have size 2})
        }

        verify(mockClown, 2)
      }
    }

    describe("(not have size N syntax)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and not have size {mockClown.hasBigRedNose; 1})
        }

        verify(mockClown, 1)

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and not {mockClown.hasBigRedNose; have size (1)})
        }

        verify(mockClown, 2)

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and {mockClown.hasBigRedNose; not have size (1)})
        }

        verify(mockClown, 3)
      }
    }

    describe("(equal N syntax)") {
      it("should not short-circuit if left matcher doesn't match") {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (equal ("ho") and equal {mockClown.hasBigRedNose; "ho"})
        }

        verify(mockClown, 1)

        intercept[TestFailedException] {
          "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
        }

        verify(mockClown, 2)
      }
    }
  }

  describe("(not equal N syntax)") {
    it("should not short-circuit if left matcher doesn't match") {

      val mockClown = mock[Clown]

      intercept[TestFailedException] {
        "hi" should (equal ("ho") and not equal {mockClown.hasBigRedNose; "ho"})
      }

      verify(mockClown, 1)

      intercept[TestFailedException] {
        "hi" should (equal ("ho") and {mockClown.hasBigRedNose; not equal ("ho")})
      }

      verify(mockClown, 2)
    }
  }

  describe("Matcher expressions to the right of or") {
    describe("(A plain-old matcher)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or { mockClown.hasBigRedNose; have length (2) })
 
        verify(mockClown, 1)
      }
    }

    describe("(have length N syntax)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or have length {mockClown.hasBigRedNose; 2})

        verify(mockClown, 1)

        "hi" should (have length (2) or {mockClown.hasBigRedNose; have length 2})

        verify(mockClown, 2)
      }
    }

    describe("(not have length N syntax)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or not have length {mockClown.hasBigRedNose; 1})

        verify(mockClown, 1)

        "hi" should (have length (2) or not {mockClown.hasBigRedNose; have length (1)})

        verify(mockClown, 2)

        "hi" should (have length (2) or {mockClown.hasBigRedNose; not have length (1)})

        verify(mockClown, 3)
      }
    }

    describe("(have size N syntax)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        Array(1, 2) should (have size (2) or have size {mockClown.hasBigRedNose; 2})

        verify(mockClown, 1)

        Array(1, 2) should (have size (2) or {mockClown.hasBigRedNose; have size 2})

        verify(mockClown, 2)
      }
    }

    describe("(not have size N syntax)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        Array(1, 2) should (have size (2) or not have size {mockClown.hasBigRedNose; 1})

        verify(mockClown, 1)

        Array(1, 2) should (have size (2) or not {mockClown.hasBigRedNose; have size (1)})

        verify(mockClown, 2)

        Array(1, 2) should (have size (2) or {mockClown.hasBigRedNose; not have size (1)})

        verify(mockClown, 3)
      }
    }

    describe("(equal N syntax)") {
      it("should not short-circuit if left matcher does match") {

        val mockClown = mock[Clown]

        "hi" should (equal ("hi") or equal {mockClown.hasBigRedNose; "ho"})

        verify(mockClown, 1)

        "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})

        verify(mockClown, 2)
      }
    }

    describe("(be >/</>=/<= syntax)") {
      it("should not short-circuit if left matcher does match for <") {

        val mockClown = mock[Clown]

        5 should (be < (7) or be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, 1)

        5 should (be < (7) or be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, 2)
      }
      it("should not short-circuit if left matcher does match for >") {

        val mockClown = mock[Clown]

        5 should (be > (3) or be > {mockClown.hasBigRedNose; 3})

        verify(mockClown, 1)

        5 should (be > (3) or be > {mockClown.hasBigRedNose; 3})

        verify(mockClown, 2)
      }
    }

    describe("(not equal N syntax)") {
      it("should not short-circuit if left matcher does match for <") {

        val mockClown = mock[Clown]

        5 should (be < (3) or not be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, 1)

        5 should (be < (3) or not be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, 2)
      }
      it("should not short-circuit if left matcher does match for >") {

        val mockClown = mock[Clown]

        5 should (be > (7) or not be > {mockClown.hasBigRedNose; 8})

        verify(mockClown, 1)

        5 should (be > (7) or not be > {mockClown.hasBigRedNose; 8})

        verify(mockClown, 2)
      }
    }
  }
}
