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
import org.scalatest.exceptions.NotAllowedException
import Matchers._
import SharedHelpers.thisLineNumber

class ShouldBeTripleEqualsSpec extends Spec with Checkers with ReturnsNormallyThrowsAssertion {

  val fileName = "ShouldBeTripleEqualsSpec.scala"

  // Checking for a specific size
  object `The 'be === (x)' syntax` {

    object `when used with Arrays` {
      def `should throw NotAllowedException` {
        val e = intercept[NotAllowedException] {
          Array(1, 2) should be === Array(1, 2)
        }
        assert(e.message === Some(Resources("beTripleEqualsNotAllowed")))
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    object `when used with nulls` {
      def `should throw NotAllowedException` {
        val s: String = null
        val e = intercept[NotAllowedException] {
          s should be === Array(1, 2)
        }
        assert(e.message === Some(Resources("beTripleEqualsNotAllowed")))
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    object `on Int` {
      def `should throw NotAllowedException` {
        val s: String = null
        val e = intercept[NotAllowedException] {
          1 should be === 2
        }
        assert(e.message === Some(Resources("beTripleEqualsNotAllowed")))
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    object `on String` {
      def `should throw NotAllowedException` {
        val s: String = null
        val e = intercept[NotAllowedException] {
          "a" should be === "a"
        }
        assert(e.message === Some(Resources("beTripleEqualsNotAllowed")))
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}
