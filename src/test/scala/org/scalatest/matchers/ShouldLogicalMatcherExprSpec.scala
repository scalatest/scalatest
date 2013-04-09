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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.prop.Checkers
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar
import org.scalatest.exceptions.TestFailedException

class ShouldLogicalMatcherExprSpec extends Spec with ShouldMatchers with Checkers with MockitoSugar with ReturnsNormallyThrowsAssertion {

  class Clown {
    def hasBigRedNose = true
  }

  object `Matcher expressions to the right of and` {
    object `(A plain-old matcher)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and { mockClown.hasBigRedNose; have length (2) })
        }
 
        verify(mockClown, times(1)).hasBigRedNose
      }
    }

    object `(have length N syntax)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and have length {mockClown.hasBigRedNose; 2})
        }

        verify(mockClown, times(1)).hasBigRedNose

        intercept[TestFailedException] {
          "hi" should (have length (1) and {mockClown.hasBigRedNose; have length 2})
        }

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(not have length N syntax)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (have length (1) and not have length {mockClown.hasBigRedNose; 1})
        }

        verify(mockClown, times(1)).hasBigRedNose

        intercept[TestFailedException] {
          "hi" should (have length (1) and not {mockClown.hasBigRedNose; have length (1)})
        }

        verify(mockClown, times(2)).hasBigRedNose

        intercept[TestFailedException] {
          "hi" should (have length (1) and {mockClown.hasBigRedNose; not have length (1)})
        }

        verify(mockClown, times(3)).hasBigRedNose
      }
    }

    object `(have size N syntax)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and have size {mockClown.hasBigRedNose; 2})
        }

        verify(mockClown, times(1)).hasBigRedNose

        intercept[TestFailedException] {
          "hi" should (have size (1) and {mockClown.hasBigRedNose; have size 2})
        }

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(not have size N syntax)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and not have size {mockClown.hasBigRedNose; 1})
        }

        verify(mockClown, times(1)).hasBigRedNose

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and not {mockClown.hasBigRedNose; have size (1)})
        }

        verify(mockClown, times(2)).hasBigRedNose

        intercept[TestFailedException] {
          Array(1, 2) should (have size (1) and {mockClown.hasBigRedNose; not have size (1)})
        }

        verify(mockClown, times(3)).hasBigRedNose
      }
    }

    object `(equal N syntax)` {
      def `should not short-circuit if left matcher doesn't match` {

        val mockClown = mock[Clown]

        intercept[TestFailedException] {
          "hi" should (equal ("ho") and equal {mockClown.hasBigRedNose; "ho"})
        }

        verify(mockClown, times(1)).hasBigRedNose

        intercept[TestFailedException] {
          "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
        }

        verify(mockClown, times(2)).hasBigRedNose
      }
    }
  }

  object `(not equal N syntax)` {
    def `should not short-circuit if left matcher doesn't match` {

      val mockClown = mock[Clown]

      intercept[TestFailedException] {
        "hi" should (equal ("ho") and not equal {mockClown.hasBigRedNose; "ho"})
      }

      verify(mockClown, times(1)).hasBigRedNose

      intercept[TestFailedException] {
        "hi" should (equal ("ho") and {mockClown.hasBigRedNose; not equal ("ho")})
      }

      verify(mockClown, times(2)).hasBigRedNose
    }
  }

  object `Matcher expressions to the right of or` {
    object `(A plain-old matcher)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or { mockClown.hasBigRedNose; have length (2) })
 
        verify(mockClown, times(1)).hasBigRedNose
      }
    }

    object `(have length N syntax)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or have length {mockClown.hasBigRedNose; 2})

        verify(mockClown, times(1)).hasBigRedNose

        "hi" should (have length (2) or {mockClown.hasBigRedNose; have length 2})

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(not have length N syntax)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        "hi" should (have length (2) or not have length {mockClown.hasBigRedNose; 1})

        verify(mockClown, times(1)).hasBigRedNose

        "hi" should (have length (2) or not {mockClown.hasBigRedNose; have length (1)})

        verify(mockClown, times(2)).hasBigRedNose

        "hi" should (have length (2) or {mockClown.hasBigRedNose; not have length (1)})

        verify(mockClown, times(3)).hasBigRedNose
      }
    }

    object `(have size N syntax)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        Array(1, 2) should (have size (2) or have size {mockClown.hasBigRedNose; 2})

        verify(mockClown, times(1)).hasBigRedNose

        Array(1, 2) should (have size (2) or {mockClown.hasBigRedNose; have size 2})

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(not have size N syntax)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        Array(1, 2) should (have size (2) or not have size {mockClown.hasBigRedNose; 1})

        verify(mockClown, times(1)).hasBigRedNose

        Array(1, 2) should (have size (2) or not {mockClown.hasBigRedNose; have size (1)})

        verify(mockClown, times(2)).hasBigRedNose

        Array(1, 2) should (have size (2) or {mockClown.hasBigRedNose; not have size (1)})

        verify(mockClown, times(3)).hasBigRedNose
      }
    }

    object `(equal N syntax)` {
      def `should not short-circuit if left matcher does match` {

        val mockClown = mock[Clown]

        "hi" should (equal ("hi") or equal {mockClown.hasBigRedNose; "ho"})

        verify(mockClown, times(1)).hasBigRedNose

        "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(be >/</>=/<= syntax)` {
      def `should not short-circuit if left matcher does match for <` {

        val mockClown = mock[Clown]

        5 should (be < (7) or be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(1)).hasBigRedNose

        5 should (be < (7) or be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(2)).hasBigRedNose
      }
      def `should not short-circuit if left matcher does match for >` {

        val mockClown = mock[Clown]

        5 should (be > (3) or be > {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(1)).hasBigRedNose

        5 should (be > (3) or be > {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(2)).hasBigRedNose
      }
    }

    object `(not equal N syntax)` {
      def `should not short-circuit if left matcher does match for <` {

        val mockClown = mock[Clown]

        5 should (be < (3) or not be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(1)).hasBigRedNose

        5 should (be < (3) or not be < {mockClown.hasBigRedNose; 3})

        verify(mockClown, times(2)).hasBigRedNose
      }
      def `should not short-circuit if left matcher does match for >` {

        val mockClown = mock[Clown]

        5 should (be > (7) or not be > {mockClown.hasBigRedNose; 8})

        verify(mockClown, times(1)).hasBigRedNose

        5 should (be > (7) or not be > {mockClown.hasBigRedNose; 8})

        verify(mockClown, times(2)).hasBigRedNose
      }
    }
  }
}
