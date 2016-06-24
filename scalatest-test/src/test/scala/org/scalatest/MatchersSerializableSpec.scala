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

import matchers.{AMatcher, AnMatcher}
import exceptions.TestFailedException
import SharedHelpers.serializeRoundtrip
import org.scalatest.matchers.{BePropertyMatchResult, HavePropertyMatchResult, HavePropertyMatcher, _}

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

  }

}