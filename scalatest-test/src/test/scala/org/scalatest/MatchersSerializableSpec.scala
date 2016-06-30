/*
 * Copyright 2001-2016 Artima, Inc.
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

import matchers.{AMatcher, AnMatcher, Matcher}
import exceptions.TestFailedException
import SharedHelpers.serializeRoundtrip
import org.scalatest.matchers.{BePropertyMatchResult, HavePropertyMatchResult, HavePropertyMatcher, _}
import enablers.Existence

class MatchersSerializableSpec extends FunSpec {

  import Matchers._

  describe("Matchers") {

    it("'a should be a AMatcher' syntax should produce Serializable TestFailedException") {
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0 }
      val e = intercept[TestFailedException] {
        -1 should be a positiveNumber
      }
      serializeRoundtrip(e)
    }

    it("'a should be a AnMatcher' syntax should produce Serializable TestFailedException") {
      val evenNumber = AnMatcher[Int]("even number") { _ % 2 == 0 }
      val e = intercept[TestFailedException] {
        11 should be an evenNumber
      }
      serializeRoundtrip(e)
    }

    it("'a should be theSameInstanceAs b' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test 1" should be theSameInstanceAs "test 2"
      }
      serializeRoundtrip(e)
    }

    // SKIP-SCALATESTJS-START
    it("'a should be a 'file' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        class NotFileMock extends Serializable {
          def file: Boolean = false
          def directory: Boolean = true
          def exists: Boolean = true
          override def toString = "NotFileMock"
        }
        (new NotFileMock) should be a 'file
      }
      serializeRoundtrip(e)
    }

    it("'a should be an 'file' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        class NotFileMock extends Serializable {
          def file: Boolean = false
          def directory: Boolean = true
          def exists: Boolean = true
          override def toString = "NotFileMock"
        }
        (new NotFileMock) should be an 'file
      }
      serializeRoundtrip(e)
    }
    // SKIP-SCALATESTJS-END

    it("'a should be a BePropertyMatcher' should produce Serializable TestFailedException") {

      class NonEmptyStringBePropertyMatcher extends BePropertyMatcher[String] {
        def apply(value: String) = {
          new BePropertyMatchResult(!value.isEmpty, "non-empty string")
        }
      }

      val e = intercept[TestFailedException] {
        "" should be a (new NonEmptyStringBePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'a should be an BePropertyMatcher' should produce Serializable TestFailedException") {

      class EmptyStringBePropertyMatcher extends BePropertyMatcher[String] {
        def apply(value: String) = {
          new BePropertyMatchResult(value.isEmpty, "empty string")
        }
      }

      val e = intercept[TestFailedException] {
        "test" should be an (new EmptyStringBePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'a should be definedAt b' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        List(1, 2, 3) should be definedAt 4
      }
      serializeRoundtrip(e)
    }

    it("'a should include regex (\"a(b*)c\" withGroup \"bb\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should include regex ("a(b*)c" withGroup "bb")
      }
      serializeRoundtrip(e)
    }

    it("'a should include regex (\"a(b*)c\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should include regex ("a(b*)c")
      }
      serializeRoundtrip(e)
    }

    it("'a should startWith regex (\"a(b*)c\" withGroup \"bb\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should startWith regex ("a(b*)c" withGroup "bb")
      }
      serializeRoundtrip(e)
    }

    it("'a should startWith regex (\"a(b*)c\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should startWith regex ("a(b*)c")
      }
      serializeRoundtrip(e)
    }

    it("'a should endWith regex (\"a(b*)c\" withGroup \"bb\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should endWith regex ("a(b*)c" withGroup "bb")
      }
      serializeRoundtrip(e)
    }

    it("'a should endWith regex (\"a(b*)c\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should endWith regex ("a(b*)c")
      }
      serializeRoundtrip(e)
    }

    it("'a should fullyMatch regex (\"a(b*)c\" withGroup \"bb\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      serializeRoundtrip(e)
    }

    it("'a should fullyMatch regex (\"a(b*)c\")' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should fullyMatch regex ("a(b*)c")
      }
      serializeRoundtrip(e)
    }

    it("'a should have length (2L)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should have length (2L)
      }
      serializeRoundtrip(e)
    }

    it("'a should have size (2L)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should have size (2L)
      }
      serializeRoundtrip(e)
    }

    it("'a should have message (xxx)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        new RuntimeException("test") should have message ("testing")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not equal (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not equal (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be <= (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be <= (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be >= (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be >= (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be < (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be < (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be > (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be > (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be (BeMatcher)' should produce Serializable TestFailedException") {
      val beMatcher = BeMatcher[Int] { list =>
        MatchResult(true, "test", "test")
      }
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be (beMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val bePropertyMatcher = BePropertyMatcher[Int] { list =>
        BePropertyMatchResult(true, "test")
      }
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be (bePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be a (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val bePropertyMatcher = BePropertyMatcher[Int] { list =>
        BePropertyMatchResult(true, "test")
      }
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be a (bePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be an (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val bePropertyMatcher = BePropertyMatcher[Int] { list =>
        BePropertyMatchResult(true, "test")
      }
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should not be an (bePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be theSameInstanceAs (b)' should produce Serializable TestFailedException") {
      val b = "b"
      val e = intercept[TestFailedException] {
        all(List("a", b, "c")) should not be theSameInstanceAs (b)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be definedAt (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(3))) should not be definedAt (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be have length (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test1", "test2", "test3")) should not have length (5)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be have size (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test1", "test2", "test3")) should not have size (5)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be have (HavePropertyMatcher)' should produce Serializable TestFailedException") {
      case class Book(title: String, author: String)
      def title(expectedValue: String) =
        new HavePropertyMatcher[Book, String] {
          def apply(book: Book) =
            HavePropertyMatchResult(
              book.title == expectedValue,
              "title",
              expectedValue,
              book.title
            )
        }
      val e = intercept[TestFailedException] {
        all(List(Book("test1", "author1"), Book("test2", "author2"), Book("test3", "author3"))) should not have (title ("test1"))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test1", null, "test3")) should not be (null)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be (symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "", "test2")) should not be ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be a (symbol)' should produce Serializable TestFailedException") {
      case class File(isFile: Boolean)
      val e = intercept[TestFailedException] {
        all(List("test1", "", "test2")) should not be (a ('empty))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be an (symbol)' should produce Serializable TestFailedException") {
      case class File(isFile: Boolean)
      val e = intercept[TestFailedException] {
        all(List("test1", "", "test2")) should not be (an ('empty))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be sorted' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(3, 2, 1), List(1, 2, 3))) should not be sorted
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be readable' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isReadable: Boolean)
        all(List(File(false), File(true), File(false))) should not be readable
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be writable' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isWritable: Boolean)
        all(List(File(false), File(true), File(false))) should not be writable
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be empty' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test1", "", "test2")) should not be empty
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not be defined' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(None, Some(1), None)) should not be defined
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List("test"), List(null), List("test 2"))) should not contain null
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain 2
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain oneOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain oneOf (2, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain oneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain oneElementOf (List(2, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain atLeastOneOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain atLeastOneOf (2, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain atLeastOneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain atLeastOneElementOf (List(2, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain noneOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain noneOf (4, 5)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain noElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain noElementsOf (List(4, 5))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain theSameElementsAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain theSameElementsAs (List(2, 1))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain theSameElementsInOrderAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain theSameElementsInOrderAs (List(1, 2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain only (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain only (2, 1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain inOrderOnly (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain inOrderOnly (1, 2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain allOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain allOf (3, 1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain allElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain allElementsOf (List(3, 1))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain inOrder (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain inOrder (1, 2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain inOrderElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain inOrderElementsOf (List(1, 2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain atMostOneOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain atMostOneOf (1, 2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain atMostOneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(1, 3))) should not contain atMostOneElementOf (List(1, 2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain key (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(Map(1 -> 1), Map(1 -> 1, 2 -> 2), Map(1 -> 1, 3 -> 3))) should not contain key (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not contain value (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(Map(1 -> 1), Map(1 -> 1, 2 -> 2), Map(1 -> 1, 3 -> 3))) should not contain value (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not startWith (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not startWith ("dan")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not startWith regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not startWith regex ("dan*")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not endWith (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not endWith ("!")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not endWith regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not endWith regex ("st")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not include (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not include ("!")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not include regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not include regex ("st")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not fullyMatch regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "dang!", "yes")) should not fullyMatch regex ("yes")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain oneOf (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain oneOf (1, 2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain oneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain oneElementOf (List(1, 2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain atLeastOneOf (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain atLeastOneOf (1, 4)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain atLeastOneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain atLeastOneElementOf (List(1, 4))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain noneOf (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain noneOf (1, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain noElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain noElementsOf (List(1, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain theSameElementsAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain theSameElementsAs (List(1, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain theSameElementsInOrderAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain theSameElementsInOrderAs (List(1, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain only (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain only (1, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain inOrderOnly (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain inOrderOnly (1, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain allOf (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain allOf (1, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain allElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain allElementsOf (List(1, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain inOrder (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain inOrder (1, 3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain inOrderElementsOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain inOrderElementsOf (List(1, 3))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain atMostOneOf (b, c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain atMostOneOf (1, 2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain atMostOneElementOf (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1), List(1, 2), List(2, 3))) should contain atMostOneElementOf (List(1, 2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain key (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(Map(1 -> 1), Map(1 -> 1, 2 -> 2), Map(2 -> 2, 3 -> 3))) should contain key (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should contain value (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(Map(1 -> 1), Map(1 -> 1, 2 -> 2), Map(2 -> 2, 3 -> 3))) should contain value (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be theSameInstanceAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val test = "test"
        all(List(test, "other", "ha!")) should be theSameInstanceAs (test)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be a (Symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("", "other", "ha!")) should be a ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be an (Symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("", "other", "ha!")) should be an ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be a (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val bePropertyMatcher = BePropertyMatcher[String] { list =>
          BePropertyMatchResult(false, "test")
        }
        all(List("test", "other", "ha!")) should be a (bePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be an (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val bePropertyMatcher = BePropertyMatcher[String] { list =>
          BePropertyMatchResult(false, "test")
        }
        all(List("test", "other", "ha!")) should be an (bePropertyMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be definedAt (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val bePropertyMatcher = BePropertyMatcher[String] { list =>
          BePropertyMatchResult(false, "test")
        }
        all(List(List(1, 2), List(1), List(1, 2, 3))) should be definedAt (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should be (Matcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val matcher: Matcher[Int] = be (1)
        all(List(1, 2, 3)) should (matcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldEqual (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val matcher = be (1)
        all(List(1, 2, 3)) shouldEqual (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldEqual (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldEqual (7.0 +- 0.1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (sorted)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List(1, 2), List(2, 3), List(4, 3))) shouldBe (sorted)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (readable)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isReadable: Boolean)
        all(List(File(false), File(false), File(false))) shouldBe (readable)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (writable)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isWritable: Boolean)
        all(List(File(false), File(false), File(false))) shouldBe (writable)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (empty)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isWritable: Boolean)
        all(List(List.empty, List(1), List.empty)) shouldBe (empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (defined)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(Some(1), None, Some(2))) shouldBe (defined)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe a [Type]' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("1", 2, "3")) shouldBe a [String]
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe an [Type]' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("1", 2, "3")) shouldBe an [String]
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldEqual (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(null, "test", null)) shouldEqual (null)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should equal (1)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should equal (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (1)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe (1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe < (3)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe < (3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe <= (2)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe <= (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe > (3)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe > (3)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe >= (2)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe >= (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (BeMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val beMatcher = BeMatcher[Int] { list =>
          MatchResult(false, "test", "test")
        }
        all(List(1, 2, 3)) shouldBe (beMatcher)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldBe (7.0 +- 0.1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe theSameInstanceAs (b)' should produce Serializable TestFailedException") {
      val test = "test"
      val e = intercept[TestFailedException] {
        all(List(test, "ok", "done")) shouldBe theSameInstanceAs (test)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe ('empty)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List.empty, List("ok"), List.empty)) shouldBe ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe a ('empty)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List.empty, List("ok"), List.empty)) shouldBe a ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe an ('empty)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(List.empty, List("ok"), List.empty)) shouldBe an ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", null, "test 2")) shouldBe (null)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val notEmpty = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        all(List("test", null, "test 2")) shouldBe (notEmpty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe a (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val notEmpty = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        all(List("test", null, "test 2")) shouldBe a (notEmpty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldBe an (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val notEmpty = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        all(List("test", null, "test 2")) shouldBe an (notEmpty)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldNot (be (b))' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldNot (be (2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldNot (equal (b))' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) shouldNot (equal (2))
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldNot === (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should === (2)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldNot === (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List(1, 2, 3)) should === (7 +- 1)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should exist' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        all(List(File(true), File(false), File(true))) should exist
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should not (exist)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        all(List(File(true), File(false), File(true))) should not (exist)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) shouldNot (exist)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        all(List(File(true), File(false), File(true))) shouldNot (exist)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should have length (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should have length (4)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should have size (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should have size (4)
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should startWith regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should startWith regex ("hel*o")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should include regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should include regex ("hel*o")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should endWith regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should endWith regex ("hel*o")
      }
      serializeRoundtrip(e)
    }

    it("'all(a) should fullyMatch regex (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        all(List("test", "testing", "test test")) should fullyMatch regex ("hel*o")
      }
      serializeRoundtrip(e)
    }

    it("'a should be (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should be ("test 1")
      }
      serializeRoundtrip(e)
    }

    it("'a should equal (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should equal ("test 1")
      }
      serializeRoundtrip(e)
    }

    it("'a should (equal (b) and have length c)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should (equal ("test") and have length 3)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldEqual (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldEqual ("test 1")
      }
      serializeRoundtrip(e)
    }

    it("'a shouldEqual (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        14.0 shouldEqual (3.0 +- 0.5)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldEqual (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldEqual (null)
      }
      serializeRoundtrip(e)
    }

    it("'a should === (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" should === ("test 1")
      }
      serializeRoundtrip(e)
    }

    it("'a should === (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        14 should === (3 +- 1)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe ("testing")
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe < (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        2 shouldBe < (1)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe <= (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        2 shouldBe <= (1)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe > (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        1 shouldBe > (2)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe >= (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        1 shouldBe >= (2)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (BeMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val odd = BeMatcher[Int] { v =>
          MatchResult(v % 2 != 0, "test", "test")
        }
        2 shouldBe (odd)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (Spread)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        2 shouldBe (5 +- 1)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (sorted)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        List(2, 3, 1) shouldBe (sorted)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe a [Type]' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe a [BigDecimal]
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe an [Type]' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe an [BigDecimal]
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe readable' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isReadable: Boolean)
        File(false) shouldBe readable
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe writable' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isWritable: Boolean)
        File(false) shouldBe writable
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe empty' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe empty
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe defined' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val opt: Option[String] = None
        opt shouldBe defined
      }
      serializeRoundtrip(e)
    }

    it("'a shouldNot (be (b))' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        1 shouldNot (be (1))
      }
      serializeRoundtrip(e)
    }

    it("'a shouldNot (equal (b))' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        1 shouldNot (equal (1))
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (null)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe (null)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe theSameInstanceAs (b)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe theSameInstanceAs ("testing")
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (Symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe a (Symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe a ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe an (Symbol)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "test" shouldBe an ('empty)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val nonEmpty = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        "" shouldBe (nonEmpty)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe a (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val nonEmptyString = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        "" shouldBe a (nonEmptyString)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldBe an (BePropertyMatcher)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        val nonEmptyString = new BePropertyMatcher[String] {
          def apply(value: String) = {
            new BePropertyMatchResult(!value.isEmpty, "non-empty string")
          }
        }
        "" shouldBe an (nonEmptyString)
      }
      serializeRoundtrip(e)
    }

    it("'a should (exist)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        File(false) should (exist)
      }
      serializeRoundtrip(e)
    }

    it("'a should not (exist)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        File(true) should not (exist)
      }
      serializeRoundtrip(e)
    }

    it("'a shouldNot (exist)' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        case class File(isExist: Boolean)
        implicit val existence = new Existence[File] {
          def exists(file: File): Boolean = file.isExist
        }
        File(true) shouldNot (exist)
      }
      serializeRoundtrip(e)
    }

    it("'s should compile' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "vaa a = 3" should compile
      }
      serializeRoundtrip(e)
    }

    it("'s shouldNot compile' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "val a = 3" shouldNot compile
      }
      serializeRoundtrip(e)
    }

    it("'s should typeCheck' should produce Serializable TestFailedException") {
      val e = intercept[TestFailedException] {
        "val a: Int = 3" shouldNot typeCheck
      }
      serializeRoundtrip(e)
    }

  }

}