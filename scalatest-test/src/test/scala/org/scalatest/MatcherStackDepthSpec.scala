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
import matchers.BeMatcher
import matchers.MatchResult

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
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should endWith regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should endWith regex "def"
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should include regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include regex "def"
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should fullyMatch regex "def"""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should fullyMatch regex "def"
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should startWith regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should startWith regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
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

  // Using custom BeMatchers
  test("6 shouldBe odd") {
    class OddMatcher extends BeMatcher[Int] {
      def apply(left: Int) =
        MatchResult(
          left % 2 == 1,
          left.toString + " was even",
          left.toString + " was odd"
        )
    }
    val odd = new OddMatcher
    val e = intercept[exceptions.TestFailedException] {
      6 shouldBe odd
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    val e2 = intercept[exceptions.TestFailedException] {
      6 should be (odd)
    }
    e2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking object identity 
  test("ref1 should be theSameInstanceAs ref2") {
    val ref1 = "hello"
    val ref2 = "world"
    val e = intercept[exceptions.TestFailedException] {
      ref1 should be theSameInstanceAs ref2
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking an object's class 

  test("result1 shouldBe a [java.util.Date]") {
    val result1 = "hello"
    val e = intercept[exceptions.TestFailedException] {
      result1 shouldBe a [java.util.Date]
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    val e2 = intercept[exceptions.TestFailedException] {
      result1 should not be a [String]
    }
    e2.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking numbers against a range 

  test("1.0 should equal (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should equal (6.9 +- 0.2)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1.0 should === (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should === (6.9 +- 0.2)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1.0 should be (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should be (6.9 +- 0.2)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1.0 shouldEqual (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 shouldEqual 6.9 +- 0.2
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("1.0 shouldBe (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 shouldBe (6.9 +- 0.2)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  // Checking for emptiness

  test("List.empty should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      List.empty should not be empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("List(1, 2, 3) shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) shouldBe empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("None should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      None should not be empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("Some(1) shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      Some(1) shouldBe empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test(""""hello" shouldBe empty""") {
    val e = intercept[exceptions.TestFailedException] {
      "hello" shouldBe empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("new java.util.HashMap[Int, Int] should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      new java.util.HashMap[Int, Int] should not be empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("new { def isEmpty = false} shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      new { def isEmpty = false} shouldBe empty
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

/*
  test("XXX") {
    val e = intercept[exceptions.TestFailedException] {
      XXX
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("XXX") {
    val e = intercept[exceptions.TestFailedException] {
      XXX
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("XXX") {
    val e = intercept[exceptions.TestFailedException] {
      XXX
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }

  test("XXX") {
    val e = intercept[exceptions.TestFailedException] {
      XXX
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
  }
*/
}

