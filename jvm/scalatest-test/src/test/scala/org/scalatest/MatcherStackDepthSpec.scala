/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalactic.StringNormalizations._
import org.scalatest.Inspectors._
import collection.JavaConverters._
import LoneElement._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MatcherStackDepthSpec extends AnyFunSuite with Matchers {

  // Checking equality

  test("0 should equal (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should equal (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("0 should === (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should === (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("0 should be (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 should be (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("0 shouldEqual (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 shouldEqual (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("0 shouldBe (1)") {
    val e = intercept[exceptions.TestFailedException] {
      0 shouldBe (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking size and length

  test(""""abc" should have length (1)""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should have length (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should have size (1)""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should have size (1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking strings 

  test(""""abc" should startWith ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should startWith ("def")
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should endWith ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should endWith ("def")
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should include ("def")""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include ("def")
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
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
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should include regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should include regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" should fullyMatch regex ("ab(c*)" withGroups ("ff"))""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" should fullyMatch regex ("ab(c*)" withGroups ("ff"))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""abc" shouldBe empty""") {
    val e = intercept[exceptions.TestFailedException] {
      "abc" shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Greater and less than 
 
  test("10 should be < 7") {
    val e = intercept[exceptions.TestFailedException] {
      10 should be < 7
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1 should be > 7") {
    val e = intercept[exceptions.TestFailedException] {
      1 should be > 7
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("10 should be <= 7") {
    val e = intercept[exceptions.TestFailedException] {
      10 should be <= 7
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1 should be >= 7") {
    val e = intercept[exceptions.TestFailedException] {
      1 should be >= 7
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking Boolean properties with be

  test(""""howdy should be an emptyString""") {
    object emptyString extends BePropertyMatcher[String] {
      def apply(left: String) = BePropertyMatchResult(left.isEmpty, "empty string")
    }
    val e = intercept[exceptions.TestFailedException] {
      "howdy" should be an emptyString
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
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
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
    val e2 = intercept[exceptions.TestFailedException] {
      6 should be (odd)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking object identity 
  test("ref1 should be theSameInstanceAs ref2") {
    val ref1 = "hello"
    val ref2 = "world"
    val e = intercept[exceptions.TestFailedException] {
      ref1 should be theSameInstanceAs ref2
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking an object's class 

  test("result1 shouldBe a [java.util.Date]") {
    val result1 = "hello"
    val e = intercept[exceptions.TestFailedException] {
      result1 shouldBe a [java.util.Date]
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
    val e2 = intercept[exceptions.TestFailedException] {
      result1 should not be a [String]
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking numbers against a range 

  test("1.0 should equal (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should equal (6.9 +- 0.2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1.0 should === (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should === (6.9 +- 0.2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1.0 should be (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 should be (6.9 +- 0.2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1.0 shouldEqual (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 shouldEqual 6.9 +- 0.2
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("1.0 shouldBe (6.9 +- 0.2)") {
    val e = intercept[exceptions.TestFailedException] {
      1.0 shouldBe (6.9 +- 0.2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Checking for emptiness

  test("List.empty should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      List.empty should not be empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3) shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("None should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      None should not be empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(1) shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      Some(1) shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""hello" shouldBe empty""") {
    val e = intercept[exceptions.TestFailedException] {
      "hello" shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("new java.util.HashMap[Int, Int] should not be empty") {
    val e = intercept[exceptions.TestFailedException] {
      new java.util.HashMap[Int, Int] should not be empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // SKIP-DOTTY-START
  test("new { def isEmpty = false} shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      new { def isEmpty = false} shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }
  // SKIP-DOTTY-END

  // Working with "containers" 

  test("List(1, 2, 3) should contain (4)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) should contain (4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Map('a' -> 1, 'b' -> 2, 'c' -> 3) should contain ('c' -> 2)") {
    val e = intercept[exceptions.TestFailedException] {
      Map('a' -> 1, 'b' -> 2, 'c' -> 3) should contain ('c' -> 2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Set(1, 2, 3) should contain (4)") {
    val e = intercept[exceptions.TestFailedException] {
      Set(1, 2, 3) should contain (4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Array(1, 2, 3) should contain (4)") {
    val e = intercept[exceptions.TestFailedException] {
      Array(1, 2, 3) should contain (4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""123" should contain ('4')""") {
    val e = intercept[exceptions.TestFailedException] {
      "123" should contain ('4')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(2) should contain (4)") {
    val e = intercept[exceptions.TestFailedException] {
      Some(2) should contain (4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""(List("Hi", "Di", "Ho") should contain ("dee")) (after being lowerCased)""") {
    val e = intercept[exceptions.TestFailedException] {
      (List("Hi", "Di", "Ho") should contain ("dee")) (after being lowerCased)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain oneOf (6, 7, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain oneOf (6, 7, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(8) should contain oneOf (5, 7, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      Some(8) should contain oneOf (5, 7, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""yowza" should contain oneOf ('b', 'c', 'd')""") {
    val e = intercept[exceptions.TestFailedException] {
      "yowza" should contain oneOf ('b', 'c', 'd')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3) should contain oneOf (2, 3, 4)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) should contain oneOf (2, 3, 4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""(Array("Doe", "Re", "Me") should contain oneOf ("X", "RAY", "BEAM")) (after being lowerCased)""") {
    val e = intercept[exceptions.TestFailedException] {
      (Array("Doe", "Re", "Me") should contain oneOf ("X", "RAY", "BEAM")) (after being lowerCased)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4) should contain oneElementOf List(5, 7, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4) should contain oneElementOf List(5, 7, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(7) should contain oneElementOf Vector(5, 77, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      Some(7) should contain oneElementOf Vector(5, 77, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""yowza" should contain oneElementOf Set('x', 'b', 'c', 'd')""") {
    val e = intercept[exceptions.TestFailedException] {
      "yowza" should contain oneElementOf Set('x', 'b', 'c', 'd')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""(Array("Doe", "Ray", "Me") should contain oneElementOf List("X", "RAX", "BEAM")) (after being lowerCased)""") {
    val e = intercept[exceptions.TestFailedException] {
      (Array("Doe", "Ray", "Me") should contain oneElementOf List("X", "RAX", "BEAM")) (after being lowerCased)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain noneOf (7, 8, 9, 1)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain noneOf (7, 8, 9, 1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(0) should contain noneOf (7, 8, 0)") {
    val e = intercept[exceptions.TestFailedException] {
      Some(0) should contain noneOf (7, 8, 0)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""1234567" should contain noneOf ('7', '8', '9')""") {
    val e = intercept[exceptions.TestFailedException] {
      "1234567" should contain noneOf ('7', '8', '9')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5, 6, 7) should contain noElementsOf List(7, 8, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5, 6, 7) should contain noElementsOf List(7, 8, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Some(8) should contain noElementsOf Vector(7, 8, 9)") {
    val e = intercept[exceptions.TestFailedException] {
      Some(8) should contain noElementsOf Vector(7, 8, 9)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""1234567" should contain noElementsOf Set('7', '8', '9')""") {
    val e = intercept[exceptions.TestFailedException] {
      "1234567" should contain noElementsOf Set('7', '8', '9')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Working with "aggregations" 

  test("List(1, 2, 3) should contain atLeastOneOf (4, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) should contain atLeastOneOf (4, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Array(1, 2, 3) should contain atLeastOneOf (4, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      Array(1, 2, 3) should contain atLeastOneOf (4, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""dog" should contain atLeastOneOf ('c', 'a', 't')""") {
    val e = intercept[exceptions.TestFailedException] {
      "dog" should contain atLeastOneOf ('c', 'a', 't')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""(Vector(" A", "B ") should contain atLeastOneOf ("d ", "o", "g")) (after being lowerCased and trimmed)""") {
    val e = intercept[exceptions.TestFailedException] {
      (Vector(" A", "B ") should contain atLeastOneOf ("d ", "o", "g")) (after being lowerCased and trimmed)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3) should contain atLeastOneElementOf List(4, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3) should contain atLeastOneElementOf List(4, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("Array(1, 2, 3) should contain atLeastOneElementOf Vector(4, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      Array(1, 2, 3) should contain atLeastOneElementOf Vector(4, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test(""""dog" should contain atLeastOneElementOf Set('c', 'a', 't')""") {
    val e = intercept[exceptions.TestFailedException] {
      "dog" should contain atLeastOneElementOf Set('c', 'a', 't')
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""(Vector(" A", "B ") should contain atLeastOneElementOf List("d ", "o", "g")) (after being lowerCased and trimmed)""") {
    val e = intercept[exceptions.TestFailedException] {
      (Vector(" A", "B ") should contain atLeastOneElementOf List("d ", "o", "g")) (after being lowerCased and trimmed)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain atMostOneOf (4, 5, 6, 7)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain atMostOneOf (4, 5, 6, 7)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain atMostOneElementOf Vector(4, 5, 6, 7)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain atMostOneElementOf Vector(4, 5, 6, 7)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain allOf (2, 3, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain allOf (2, 3, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 4, 5) should contain allElementsOf Array(2, 3, 5, 6)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 4, 5) should contain allElementsOf Array(2, 3, 5, 6)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 3, 2, 1) should contain only (1, 2, 3, 4)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 3, 2, 1) should contain only (1, 2, 3, 4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2, 3, 5)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2, 3, 5)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Working with "sequences" 

  test("List(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 3, 2)") {
    val e = intercept[exceptions.TestFailedException] {
      List(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 3, 2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrder (1, 3, 99)") {
    val e = intercept[exceptions.TestFailedException] {
      List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrder (1, 3, 99)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrderElementsOf Array(1, 2, 3, 99)") {
    val e = intercept[exceptions.TestFailedException] {
      List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrderElementsOf Array(1, 2, 3, 99)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("List(3, 2, 1) should contain theSameElementsInOrderAs collection.mutable.TreeSet(3, 2, 1)") {
    val e = intercept[exceptions.TestFailedException] {
      List(3, 2, 1) should contain theSameElementsInOrderAs collection.mutable.TreeSet(3, 2, 1)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Working with "sortables" 

  test("List(2, 1, 3) shouldBe sorted") {
    val e = intercept[exceptions.TestFailedException] {
      List(2, 1, 3) shouldBe sorted
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Working with iterators 

  test("it.toStream should contain (4)") {
    val e = intercept[exceptions.TestFailedException] {
      val it = List(1, 2, 3).iterator
      it.toStream should contain (4)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Inspector shorthands 

  test("forAll (ys) { y => y should be > 0 }") {
    val e = intercept[exceptions.TestFailedException] {
      val yss =
        List(
          List(1, 2, 3),
          List(1, 2, 3),
          List(-11, 2, 3)
        )

      forAll (yss) { ys =>
        forAll (ys) { y => y should be > 0 }
      }
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
  }

  test("all (xs) should be < 10") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(1, 2, 30)
      all (xs) should be < 10
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("atMost (2, xs) should be >= 4") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(1, 2, 3, 4, 5, 6)
      atMost (2, xs) should be >= 4
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("atLeast (3, xs) should be < 5") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(3, 4, 5, 6)
      atLeast (3, xs) should be < 5
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("between (2, 3, xs) should (be > 1 and be < 5)") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(1, 4, 5, 6)
      between (2, 3, xs) should (be > 1 and be < 5)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("exactly (2, xs) should be <= 2") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(0, 1, 2, 3, 4, 5, 6)
      exactly (2, xs) should be <= 2
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("every (xs) should be < 10") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(1, 2, 3, 4, 5, 60)
      every (xs) should be < 10
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("exactly (2, xs) shouldEqual 2") {
    val e = intercept[exceptions.TestFailedException] {
      val xs = List(1, 2, 3, 4, 5, 6)
      exactly (2, xs) shouldEqual 2
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("all (Array(1, 2, 3)) should be < 3") {
    val e = intercept[exceptions.TestFailedException] {
      all (Array(1, 2, 3)) should be < 3
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("all (js) should be < 5") {
    val e = intercept[exceptions.TestFailedException] {
      val js = List(1, 2, 3, 4, 5).asJava
      all (js) should be < 5
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""atLeast(1, jmap) shouldBe Entry("b", 2)""") {
    val e = intercept[exceptions.TestFailedException] {
      val jmap = Map("a" -> 1, "bee" -> 2).asJava
      atLeast(1, jmap) shouldBe Entry("b", 2)
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""atLeast(2, "hello, world!") shouldBe 'w'""") {
    val e = intercept[exceptions.TestFailedException] {
      atLeast(2, "hello, world!") shouldBe 'w'
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Single-element collections 

  test("set.loneElement should be <= 10") {
    val e = intercept[exceptions.TestFailedException] {
      val set = Set(1, 2)
      set.loneElement should be <= 10
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Java collections and maps 

  test("javaCollection should be ('empty)") {
    val e = intercept[exceptions.TestFailedException] {
      val javaCollection = List(1, 2, 3, 4, 5).asJava
      javaCollection should be (Symbol("empty"))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaMap should be ('empty)") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map("a" -> 1, "bee" -> 2).asJava
      javaMap should be (Symbol("empty"))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaList should have length 9") {
    val e = intercept[exceptions.TestFailedException] {
      val javaList = List(1, 2, 3, 4, 5).asJava
      javaList should have length 9
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaMap should have size 20") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map("a" -> 1, "bee" -> 2).asJava
      javaMap should have size 20
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaSet should have size 90") {
    val e = intercept[exceptions.TestFailedException] {
      val javaSet = Set("a", "bee").asJava
      javaSet should have size 90
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""javaCollection should contain ("five")""") {
    val e = intercept[exceptions.TestFailedException] {
      val javaCollection = List("a", "bee").asJava
      javaCollection should contain ("five")
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaMap should contain (Entry(2, 3))") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map(2 -> 1, 3 -> 2).asJava
      javaMap should contain (Entry(2, 3))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaMap should contain oneOf (Entry(2, 3), Entry(3, 4))") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map(2 -> 1, 3 -> 2).asJava
      javaMap should contain oneOf (Entry(2, 3), Entry(3, 4))
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("javaMap should contain key 1") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map(2 -> 1, 3 -> 2).asJava
      javaMap should contain key 1
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  test("""javaMap should contain value "Yowza"""") {
    val e = intercept[exceptions.TestFailedException] {
      val javaMap = Map(2 -> "Boy", 3 -> "Howdy").asJava
      javaMap should contain value "Yowza"
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Strings and Arrays as collections 

  // be as an equality comparison 

  // Being negative 

  // Checking that a snippet of code does not compile 

  //  Logical expressions with and and or

  // Working with Options 

  // Checking arbitrary properties with have

  // Using length and size with HavePropertyMatchers 

  // Checking that an expression matches a pattern 

  // Using custom matchers

  // Checking for expected exceptions 

  test("""an [IndexOutOfBoundsException] should be thrownBy "hi".charAt(1)""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] should be thrownBy "hi".charAt(1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] should be thrownBy "hi".charAt(1)""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] should be thrownBy "hi".charAt(1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""an [IndexOutOfBoundsException] shouldBe thrownBy { "hi".charAt(1) }""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] shouldBe thrownBy { "hi".charAt(1) }
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] shouldBe thrownBy { "hi".charAt(1) }""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] shouldBe thrownBy { "hi".charAt(1) }
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""an [IndexOutOfBoundsException] should (be thrownBy { "hi".charAt(1) })""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] should (be thrownBy { "hi".charAt(1) })
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] should (be thrownBy { "hi".charAt(1) })""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] should (be thrownBy { "hi".charAt(1) })
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""the [ArithmeticException] thrownBy 0 / 1 should have message "/ by zero"""") {
    val e = intercept[exceptions.TestFailedException] {
      the [ArithmeticException] thrownBy 0 / 1 should have message "/ by zero"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  def throwEvenInJavaScript: Int = { throw new Exception("ensure an exception on Javascript too") }

  test("""the [ArithmeticException] thrownBy throwEvenInJavaScript should have message "/ by one"""") {
    val e = intercept[exceptions.TestFailedException] {
      the [ArithmeticException] thrownBy throwEvenInJavaScript should have message "/ by one"
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("noException should be thrownBy throwEvenInJavaScript") {
    val e = intercept[exceptions.TestFailedException] {
      noException should be thrownBy throwEvenInJavaScript
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""an [IndexOutOfBoundsException] must be thrownBy "hi".charAt(1)""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] must be thrownBy "hi".charAt(1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] must be thrownBy "hi".charAt(1)""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] must be thrownBy "hi".charAt(1)
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""an [IndexOutOfBoundsException] mustBe thrownBy { "hi".charAt(1) }""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] mustBe thrownBy { "hi".charAt(1) }
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] mustBe thrownBy { "hi".charAt(1) }""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] mustBe thrownBy { "hi".charAt(1) }
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""an [IndexOutOfBoundsException] must (be thrownBy { "hi".charAt(1) })""") {
    val e = intercept[exceptions.TestFailedException] {
      an [IndexOutOfBoundsException] must (be thrownBy { "hi".charAt(1) })
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("""a [IndexOutOfBoundsException] must (be thrownBy { "hi".charAt(1) })""") {
    val e = intercept[exceptions.TestFailedException] {
      a [IndexOutOfBoundsException] must (be thrownBy { "hi".charAt(1) })
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }

  test("noException must be thrownBy throwEvenInJavaScript") {
    val e = intercept[exceptions.TestFailedException] {
      noException must be thrownBy throwEvenInJavaScript
    }
    e.failedCodeLineNumber should be (Some(thisLineNumber - 2))
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
  }
}

/*
    println("\n!!!!!!!!!!\n")
    e.printStackTrace()
    println("\n!!!!!!!!!!\n")
*/
