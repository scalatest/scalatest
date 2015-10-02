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

  test("new { def isEmpty = false} shouldBe empty") {
    val e = intercept[exceptions.TestFailedException] {
      new { def isEmpty = false} shouldBe empty
    }
    e.failedCodeFileName should be (Some("MatcherStackDepthSpec.scala"))
    e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
  }

  // Working with "containers" 

  // Working with "aggregations" 

  // Working with "sequences" 

  // Working with "sortables" 

  // Working with iterators 

  // Inspector shorthands 

  // Single-element collections 

  // Java collections and maps 

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
