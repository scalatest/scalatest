/*
 * Copyright 2001-2015 Artima, Inc.
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
import matchers.BePropertyMatcher
import matchers.BePropertyMatchResult

class MatcherStackDepthSpec extends FunSuite with Matchers {

  // Checking equality

  test("0 should equal (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should equal (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("0 should === (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should === (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("0 should be (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should be (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("0 shouldEqual (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 shouldEqual (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("0 shouldBe (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 shouldBe (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking size and length

  test(""""abc" should have length (1)""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should have length (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should have size (1)""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should have size (1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking strings 

  test(""""abc" should startWith ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should startWith ("def")
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should endWith ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should endWith ("def")
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should include ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include ("def")
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should startWith regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should startWith regex "def"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should endWith regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should endWith regex "def"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should include regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include regex "def"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should fullyMatch regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should fullyMatch regex "def"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should startWith regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should startWith regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should endWith regex ("ab(c*)") withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should endWith regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should include regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" should fullyMatch regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should fullyMatch regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""abc" shouldBe empty""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" shouldBe empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Greater and less than 
 
  test("10 should be < 7") {
    val e = intercept[exceptions.TestFailedException] {
      10 should be < 7
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1 should be > 7") {
    val e = intercept[exceptions.TestFailedException] {
      1 should be > 7
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("10 should be <= 7") {
    val e = intercept[exceptions.TestFailedException] {
      10 should be <= 7
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1 should be >= 7") {
    val e = intercept[exceptions.TestFailedException] {
      1 should be >= 7
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking Boolean properties with be

  test(""""howdy should be an emptyString""") {
    object emptyString extends BePropertyMatcher[String] {
      def apply(left: String) = BePropertyMatchResult(left.isEmpty, "empty string")
    }
    val e = intercept[exceptions.TestFailedException] {
      "howdy" should be an emptyString
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

}
