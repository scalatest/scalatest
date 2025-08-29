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

import matchers.{BeMatcher, MatchResult, BePropertyMatcher, BePropertyMatchResult}
import SharedHelpers._
import FailureMessages.decorateToStringValue
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldNotShorthandSpec extends AnyFunSpec with EmptyMocks with BookPropertyMatchers {

  private val prettifier = Prettifier.default

  describe("The shouldNot syntax") {

    // SKIP-SCALATESTJS,NATIVE-START
    it("should work with theSameInstanceAs") {
      val string = "Hi"
      val obj: AnyRef = string
      val otherString = new String("Hi")

      otherString shouldNot { be theSameInstanceAs (string) }
      otherString shouldNot be theSameInstanceAs (string)

      val caught1 = intercept[TestFailedException] {
        obj shouldNot { be theSameInstanceAs (string) }
      }
      assert(caught1.message === Some("\"Hi\" was the same instance as \"Hi\""))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught2 = intercept[TestFailedException] {
        obj shouldNot be theSameInstanceAs string
      }
      assert(caught2.message === Some("\"Hi\" was the same instance as \"Hi\""))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should work with any") {
      1 shouldNot { be (2) }
      1 shouldNot be (2)
      "hi" shouldNot be (null)

      val caught1 = intercept[TestFailedException] {
        1 shouldNot { be (1) }
      }
      assert(caught1.message === Some("1 was equal to 1"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val nullString: String = null
      val caught2 = intercept[TestFailedException] {
        nullString shouldNot be (null)
      }
      assert(caught2.message === Some("The reference was null"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[TestFailedException] {
        1 shouldNot be (1)
      }
      assert(caught3.message === Some("1 was equal to 1"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught4 = intercept[TestFailedException] {
        1 shouldNot (not (not be (1)))
      }
      assert(caught4.message === Some("1 was equal to 1"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

      6 shouldNot be > 7
      val caught5 = intercept[TestFailedException] {
        8 shouldNot be > 7
      }
      assert(caught5.message === Some("8 was greater than 7"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

      8 shouldNot be < 7
      val caught6 = intercept[TestFailedException] {
        5 shouldNot be < 7
      }
      assert(caught6.message === Some("5 was less than 7"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

      3 shouldNot be >= 7
      val caught7 = intercept[TestFailedException] {
        8 shouldNot be >= 7
      }
      assert(caught7.message === Some("8 was greater than or equal to 7"))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

      8 shouldNot be <= 7
      val caught8 = intercept[TestFailedException] {
        3 shouldNot be <= 7
      }
      assert(caught8.message === Some("3 was less than or equal to 7"))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

      true shouldNot be (false)
      val caught9 = intercept[TestFailedException] {
        true shouldNot be (true)
      }
      assert(caught9.message === Some("true was true"))
      assert(caught9.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should work with BeMatcher") {

      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd"
          )
        }
      }
      val odd = new OddMatcher
      val even = not (odd)

      2 shouldNot be (odd)
      1 shouldNot be (even)
      22 shouldNot (not (be (even)))
      1 shouldNot (not (be (odd)))

      val caught1 = intercept[TestFailedException] {
        3 shouldNot be (odd)
      }
      assert(caught1.getMessage === "3 was odd")
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught2 = intercept[TestFailedException] {
        6 shouldNot be (even)
      }
      assert(caught2.getMessage === "6 was even")
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught3 = intercept[TestFailedException] {
        6 shouldNot (not (be (odd)))
      }
      assert(caught3.getMessage === "6 was even")
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should work with symbol") {
      notEmptyMock shouldNot { be (Symbol("empty")) }
      notEmptyMock shouldNot be (Symbol("empty"))
      isNotEmptyMock shouldNot { be (Symbol("empty")) }
      isNotEmptyMock shouldNot be (Symbol("empty"))

      val ex1 = intercept[TestFailedException] {
        noPredicateMock shouldNot { be (Symbol("empty")) }
      }
      assert(ex1.message === Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      assert(ex1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(ex1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val ex2 = intercept[TestFailedException] {
        noPredicateMock shouldNot (be (Symbol("full")))
      }
      assert(ex2.message === Some("NoPredicateMock has neither a full nor an isFull method"))
      assert(ex2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(ex2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val ex3 = intercept[TestFailedException] {
        noPredicateMock shouldNot be (Symbol("empty"))
      }
      assert(ex3.message === Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      assert(ex3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(ex3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val ex4 = intercept[TestFailedException] {
        noPredicateMock shouldNot be (Symbol("full"))
      }
      assert(ex4.message === Some("NoPredicateMock has neither a full nor an isFull method"))
      assert(ex4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(ex4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    it("should work with BePropertyMatcher") {
      case class MyFile(
                         val name: String,
                         val file: Boolean,
                         val isDirectory: Boolean
                         )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }

      class DirectoryBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.isDirectory, "directory")
        }
      }

      def file = new FileBePropertyMatcher
      def directory = new DirectoryBePropertyMatcher

      val myFile = new MyFile("temp.txt", true, false)

      val book = new Book("A Tale of Two Cities", "Dickens", 1859, 45, true)
      val badBookPrettified = "Book(\"A Tale of Two Cities\", \"Dickens\", 1859, 45, true)"
      val badBook = new Book("A Tale of Two Cities", "Dickens", 1859, 45, false)

      badBook shouldNot be (goodRead)
      badBook shouldNot be a goodRead
      badBook shouldNot be an goodRead

      val caught1 = intercept[TestFailedException] {
        book shouldNot be (goodRead)
      }
      assert(caught1.message === Some(s"$badBookPrettified was goodRead"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught2 = intercept[TestFailedException] {
        book shouldNot be a goodRead
      }
      assert(caught2.message === Some(s"$badBookPrettified was a goodRead"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught3 = intercept[TestFailedException] {
        book shouldNot be an goodRead
      }
      assert(caught3.message === Some(s"$badBookPrettified was an goodRead"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught4 = intercept[TestFailedException] {
        book shouldNot (be (goodRead))
      }
      assert(caught4.message === Some(s"$badBookPrettified was goodRead"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught5 = intercept[TestFailedException] {
        book shouldNot (be a (goodRead))
      }
      assert(caught5.message === Some(s"$badBookPrettified was a goodRead"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught6 = intercept[TestFailedException] {
        book shouldNot (be an (goodRead))
      }
      assert(caught6.message === Some(s"$badBookPrettified was an goodRead"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should with +-") {
      val sevenDotOh = 7.0
      val minusSevenDotOh = -7.0
      val sevenDotOhFloat = 7.0f
      val minusSevenDotOhFloat = -7.0f
      val sevenLong = 7L
      val minusSevenLong = -7L
      val sevenInt = 7
      val minusSevenInt = -7
      val sevenShort: Short = 7
      val minusSevenShort: Short = -7
      val sevenByte: Byte = 7
      val minusSevenByte: Byte = -7

      // Double +- Double
      sevenDotOh shouldNot { be (7.5 +- 0.2) }
      sevenDotOh shouldNot be (7.5 +- 0.2)
      sevenDotOh shouldNot be (6.5 +- 0.2)
      minusSevenDotOh shouldNot { be (-7.5 +- 0.2) }
      minusSevenDotOh shouldNot be (-7.5 +- 0.2)
      minusSevenDotOh shouldNot be (-6.5 +- 0.2)

      // Double +- Float
      sevenDotOh shouldNot { be (7.5 +- 0.2f) }
      sevenDotOh shouldNot be (7.5 +- 0.2f)
      sevenDotOh shouldNot be (6.5 +- 0.2f)
      minusSevenDotOh shouldNot { be (-7.5 +- 0.2f) }
      minusSevenDotOh shouldNot be (-7.5 +- 0.2f)
      minusSevenDotOh shouldNot be (-6.5 +- 0.2f)

      // Double +- Long
      sevenDotOh shouldNot { be (10.0 +- 2L) }
      sevenDotOh shouldNot be (4.0 +- 2L)
      sevenDotOh shouldNot be (9.1 +- 2L)
      minusSevenDotOh shouldNot { be (-10.0 +- 2L) }
      minusSevenDotOh shouldNot be (-4.0 +- 2L)
      minusSevenDotOh shouldNot be (-9.1 +- 2L)

      // Double +- Int
      sevenDotOh shouldNot { be (10.0 +- 2) }
      sevenDotOh shouldNot be (4.0 +- 2)
      sevenDotOh shouldNot be (9.1 +- 2)
      minusSevenDotOh shouldNot { be (-10.0 +- 2) }
      minusSevenDotOh shouldNot be (-4.0 +- 2)
      minusSevenDotOh shouldNot be (-9.1 +- 2)

      // Double +- Short
      sevenDotOh shouldNot { be (10.0 +- 2.toShort) }
      sevenDotOh shouldNot be (4.0 +- 2.toShort)
      sevenDotOh shouldNot be (9.1 +- 2.toShort)
      minusSevenDotOh shouldNot { be (-10.0 +- 2.toShort) }
      minusSevenDotOh shouldNot be (-4.0 +- 2.toShort)
      minusSevenDotOh shouldNot be (-9.1 +- 2.toShort)

      // Double +- Byte
      sevenDotOh shouldNot { be (10.0 +- 2.toByte) }
      sevenDotOh shouldNot be (4.0 +- 2.toByte)
      sevenDotOh shouldNot be (9.1 +- 2.toByte)
      minusSevenDotOh shouldNot { be (-10.0 +- 2.toByte) }
      minusSevenDotOh shouldNot be (-4.0 +- 2.toByte)
      minusSevenDotOh shouldNot be (-9.1 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat shouldNot { be (7.5f +- 0.2f) }
      sevenDotOhFloat shouldNot be (7.5f +- 0.2f)
      sevenDotOhFloat shouldNot be (6.5f +- 0.2f)
      minusSevenDotOhFloat shouldNot { be (-7.5f +- 0.2f) }
      minusSevenDotOhFloat shouldNot be (-7.5f +- 0.2f)
      minusSevenDotOhFloat shouldNot be (-6.5f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat shouldNot { be (10.0f +- 2L) }
      sevenDotOhFloat shouldNot be (4.0f +- 2L)
      sevenDotOhFloat shouldNot be (9.1f +- 2L)
      minusSevenDotOhFloat shouldNot { be (-10.0f +- 2L) }
      minusSevenDotOhFloat shouldNot be (-4.0f +- 2L)
      minusSevenDotOhFloat shouldNot be (-9.1f +- 2L)

      // Float +- Int
      sevenDotOhFloat shouldNot { be (10.0f +- 2) }
      sevenDotOhFloat shouldNot be (4.0f +- 2)
      sevenDotOhFloat shouldNot be (9.1f +- 2)
      minusSevenDotOhFloat shouldNot { be (-10.0f +- 2) }
      minusSevenDotOhFloat shouldNot be (-4.0f +- 2)
      minusSevenDotOhFloat shouldNot be (-9.1f +- 2)

      // Float +- Short
      sevenDotOhFloat shouldNot { be (10.0f +- 2.toShort) }
      sevenDotOhFloat shouldNot be (4.0f +- 2.toShort)
      sevenDotOhFloat shouldNot be (9.1f +- 2.toShort)
      minusSevenDotOhFloat shouldNot { be (-10.0f +- 2.toShort) }
      minusSevenDotOhFloat shouldNot be (-4.0f +- 2.toShort)
      minusSevenDotOhFloat shouldNot be (-9.1f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat shouldNot { be (10.0f +- 2.toByte) }
      sevenDotOhFloat shouldNot be (4.0f +- 2.toByte)
      sevenDotOhFloat shouldNot be (9.1f +- 2.toByte)
      minusSevenDotOhFloat shouldNot { be (-10.0f +- 2.toByte) }
      minusSevenDotOhFloat shouldNot be (-4.0f +- 2.toByte)
      minusSevenDotOhFloat shouldNot be (-9.1f +- 2.toByte)

      // Long +- Long
      sevenLong shouldNot { be (10L +- 2L) }
      sevenLong shouldNot be (4L +- 2L)
      sevenLong shouldNot be (10L +- 2L)
      minusSevenLong shouldNot { be (-10L +- 2L) }
      minusSevenLong shouldNot be (-4L +- 2L)
      minusSevenLong shouldNot be (-10L +- 2L)

      // Long +- Int
      sevenLong shouldNot { be (10L +- 2) }
      sevenLong shouldNot be (4L +- 2)
      sevenLong shouldNot be (10L +- 2)
      minusSevenLong shouldNot { be (-10L +- 2) }
      minusSevenLong shouldNot be (-4L +- 2)
      minusSevenLong shouldNot be (-10L +- 2)

      // Long +- Short
      sevenLong shouldNot { be (10L +- 2.toShort) }
      sevenLong shouldNot be (4L +- 2.toShort)
      sevenLong shouldNot be (10L +- 2.toShort)
      minusSevenLong shouldNot { be (-10L +- 2.toShort) }
      minusSevenLong shouldNot be (-4L +- 2.toShort)
      minusSevenLong shouldNot be (-10L +- 2.toShort)

      // Long +- Byte
      sevenLong shouldNot { be (10L +- 2.toByte) }
      sevenLong shouldNot be (4L +- 2.toByte)
      sevenLong shouldNot be (10L +- 2.toByte)
      minusSevenLong shouldNot { be (-10L +- 2.toByte) }
      minusSevenLong shouldNot be (-4L +- 2.toByte)
      minusSevenLong shouldNot be (-10L +- 2.toByte)

      // Int +- Int
      sevenInt shouldNot { be (10 +- 2) }
      sevenInt shouldNot be (4 +- 2)
      sevenInt shouldNot be (10 +- 2)
      minusSevenInt shouldNot { be (-10 +- 2) }
      minusSevenInt shouldNot be (-4 +- 2)
      minusSevenInt shouldNot be (-10 +- 2)

      // Int +- Short
      sevenInt shouldNot { be (10 +- 2.toShort) }
      sevenInt shouldNot be (4 +- 2.toShort)
      sevenInt shouldNot be (10 +- 2.toShort)
      minusSevenInt shouldNot { be (-10 +- 2.toShort) }
      minusSevenInt shouldNot be (-4 +- 2.toShort)
      minusSevenInt shouldNot be (-10 +- 2.toShort)

      // Int +- Byte
      sevenInt shouldNot { be (10 +- 2.toByte) }
      sevenInt shouldNot be (4 +- 2.toByte)
      sevenInt shouldNot be (10 +- 2.toByte)
      minusSevenInt shouldNot { be (-10 +- 2.toByte) }
      minusSevenInt shouldNot be (-4 +- 2.toByte)
      minusSevenInt shouldNot be (-10 +- 2.toByte)

      // Short +- Short
      sevenShort shouldNot { be (10.toShort +- 2.toShort) }
      sevenShort shouldNot be (4.toShort +- 2.toShort)
      sevenShort shouldNot be (10.toShort +- 2.toShort)
      minusSevenShort shouldNot { be ((-10).toShort +- 2.toShort) }
      minusSevenShort shouldNot be ((-4).toShort +- 2.toShort)
      minusSevenShort shouldNot be ((-10).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort shouldNot { be (10.toShort +- 2.toByte) }
      sevenShort shouldNot be (4.toShort +- 2.toByte)
      sevenShort shouldNot be (10.toShort +- 2.toByte)
      minusSevenShort shouldNot { be ((-10).toShort +- 2.toByte) }
      minusSevenShort shouldNot be ((-4).toShort +- 2.toByte)
      minusSevenShort shouldNot be ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte shouldNot { be (10.toByte +- 2.toByte) }
      sevenByte shouldNot be (4.toByte +- 2.toByte)
      sevenByte shouldNot be (10.toByte +- 2.toByte)
      minusSevenByte shouldNot { be ((-10).toByte +- 2.toByte) }
      minusSevenByte shouldNot be ((-4).toByte +- 2.toByte)
      minusSevenByte shouldNot be ((-10).toByte +- 2.toByte)

      // Double +- Double
      val caught1 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 0.2)
      }
      assert(caught1.message === Some(sevenDotOh + " was 7.1 plus or minus 0.2"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 0.2f)
      }
      assert(caught2.message === Some(sevenDotOh + " was 7.1 plus or minus 0.20000000298023224"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 2L)
      }
      val offendingLine3 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught3.message === Some(sevenDotOh + " was 7.1 plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught3.message === Some(sevenDotOh + " was 7.1 plus or minus 2"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(offendingLine3))

      // Double +- Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 2)
      }
      val offendingLine4 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught4.message === Some(sevenDotOh + " was 7.1 plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught4.message === Some(sevenDotOh + " was 7.1 plus or minus 2"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(offendingLine4))

      // Double +- Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 2.toShort)
      }
      val offendingLine5 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught5.message === Some(sevenDotOh + " was 7.1 plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught5.message === Some(sevenDotOh + " was 7.1 plus or minus 2"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(offendingLine5))

      // Double +- Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh shouldNot be (7.1 +- 2.toByte)
      }
      val offendingLine6 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught6.message === Some(sevenDotOh + " was 7.1 plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught6.message === Some(sevenDotOh + " was 7.1 plus or minus 2"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(offendingLine6))

      // Float +- Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat shouldNot be (7.1f +- 0.2f)
      }
      val offendingLine7 = thisLineNumber - 2
      assert(caught7.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus " + 0.2f))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(offendingLine7))

      // Float +- Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat shouldNot be (7.1f +- 2L)
      }
      val offendingLine8 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught8.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught8.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2"))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(offendingLine8))

      // Float +- Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat shouldNot be (7.1f +- 2)
      }
      val offendingLine9 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught9.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught9.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2"))
      assert(caught9.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(offendingLine9))

      // Float +- Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat shouldNot be (7.1f +- 2.toShort)
      }
      val offendingLine10 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught10.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught10.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2"))
      assert(caught10.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(offendingLine10))

      // Float +- Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat shouldNot be (7.1f +- 2.toByte)
      }
      val offendingLine11 = thisLineNumber - 2
      // SKIP-SCALATESTJS,NATIVE-START
      assert(caught11.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2.0"))
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY assert(caught11.message === Some(sevenDotOh + " was " + 7.1f + " plus or minus 2"))
      assert(caught11.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(offendingLine11))

      // Long +- Long
      val caught12 = intercept[TestFailedException] {
        sevenLong shouldNot be (9L +- 2L)
      }
      assert(caught12.message === Some("7 was 9 plus or minus 2"))
      assert(caught12.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Int
      val caught13 = intercept[TestFailedException] {
        sevenLong shouldNot be (9L +- 2)
      }
      assert(caught13.message === Some("7 was 9 plus or minus 2"))
      assert(caught13.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Short
      val caught14 = intercept[TestFailedException] {
        sevenLong shouldNot be (9L +- 2.toShort)
      }
      assert(caught14.message === Some("7 was 9 plus or minus 2"))
      assert(caught14.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong shouldNot be (9L +- 2.toByte)
      }
      assert(caught15.message === Some("7 was 9 plus or minus 2"))
      assert(caught15.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Int
      val caught16 = intercept[TestFailedException] {
        sevenInt shouldNot be (9 +- 2)
      }
      assert(caught16.message === Some("7 was 9 plus or minus 2"))
      assert(caught16.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Short
      val caught17 = intercept[TestFailedException] {
        sevenInt shouldNot be (9 +- 2.toShort)
      }
      assert(caught17.message === Some("7 was 9 plus or minus 2"))
      assert(caught17.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt shouldNot be (9 +- 2.toByte)
      }
      assert(caught18.message === Some("7 was 9 plus or minus 2"))
      assert(caught18.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Short
      val caught19 = intercept[TestFailedException] {
        sevenShort shouldNot be (9.toShort +- 2.toShort)
      }
      assert(caught19.message === Some("7 was 9 plus or minus 2"))
      assert(caught19.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort shouldNot be (9.toShort +- 2.toByte)
      }
      assert(caught20.message === Some("7 was 9 plus or minus 2"))
      assert(caught20.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Byte +- Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte shouldNot be (9.toByte +- 2.toByte)
      }
      assert(caught21.message === Some("7 was 9 plus or minus 2"))
      assert(caught21.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with fullyMatch regex withGroup and withGroups") {
      
      "abbbc" shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      "abbbc" shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "abbbc" shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      "abbbc" shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val caught1 = intercept[TestFailedException] {
        "abbc" shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[TestFailedException] {
        "abbcc" shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[TestFailedException] {
        "abbc" shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught3.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[TestFailedException] {
        "abbcc" shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with startWith regex withGroup and withGroups") {
      
      "abbbc" shouldNot startWith regex ("a(b*)c" withGroup "bb")
      "abbbc" shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "abbbcdef" shouldNot startWith regex ("a(b*)c" withGroup "bb")
      "abbbcdef" shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      "abbbc" shouldNot startWith regex ("a(b*)c".r withGroup "bb")
      "abbbc" shouldNot startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      "abbbcdef" shouldNot startWith regex ("a(b*)c" withGroup "bb")
      "abbbcdef" shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      val caught1 = intercept[TestFailedException] {
        "abbc" shouldNot startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[TestFailedException] {
        "abbcc" shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some("\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[TestFailedException] {
        "abbcdef" shouldNot startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some("\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[TestFailedException] {
        "abbccdef" shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some("\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught5 = intercept[TestFailedException] {
        "abbc" shouldNot startWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught5.message === Some("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught6 = intercept[TestFailedException] {
        "abbcc" shouldNot startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some("\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught7 = intercept[TestFailedException] {
        "abbcdef" shouldNot startWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught7.message === Some("\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught8 = intercept[TestFailedException] {
        "abbccdef" shouldNot startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some("\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with endWith regex withGroup and withGroups") {
      
      "abbbc" shouldNot endWith regex ("a(b*)c" withGroup "bb")
      "abbbc" shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "123abbbc" shouldNot endWith regex ("a(b*)c" withGroup "bb")
      "123abbbc" shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      "abbbc" shouldNot endWith regex ("a(b*)c".r withGroup "bb")
      "abbbc" shouldNot endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      "123abbbc" shouldNot endWith regex ("a(b*)c" withGroup "bb")
      "123abbbc" shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      val caught1 = intercept[TestFailedException] {
        "abbc" shouldNot endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[TestFailedException] {
        "abbcc" shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some("\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[TestFailedException] {
        "123abbc" shouldNot endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some("\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[TestFailedException] {
        "123abbcc" shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some("\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught5 = intercept[TestFailedException] {
        "abbc" shouldNot endWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught5.message === Some("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught6 = intercept[TestFailedException] {
        "abbcc" shouldNot endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some("\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught7 = intercept[TestFailedException] {
        "123abbc" shouldNot endWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught7.message === Some("\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught8 = intercept[TestFailedException] {
        "123abbcc" shouldNot endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some("\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with include regex withGroup and withGroups") {
      
      "abbbc" shouldNot include regex ("a(b*)c" withGroup "bb")
      "abbbc" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "123abbbc" shouldNot include regex ("a(b*)c" withGroup "bb")
      "123abbbc" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "abbbcdef" shouldNot include regex ("a(b*)c" withGroup "bb")
      "abbbcdef" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      "123abbbcdef" shouldNot include regex ("a(b*)c" withGroup "bb")
      "123abbbcdef" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      "abbbc" shouldNot include regex ("a(b*)c".r withGroup "bb")
      "abbbc" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      "123abbbc" shouldNot include regex ("a(b*)c".r withGroup "bb")
      "123abbbc" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      "abbbcdef" shouldNot include regex ("a(b*)c".r withGroup "bb")
      "abbbcdef" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      "123abbbcdef" shouldNot include regex ("a(b*)c".r withGroup "bb")
      "123abbbcdef" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val caught1 = intercept[TestFailedException] {
        "abbc" shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some("\"abbc\" included substring that matched regex a(b*)c and group bb"))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught2 = intercept[TestFailedException] {
        "abbcc" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some("\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught3 = intercept[TestFailedException] {
        "123abbcc" shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some("\"123abbcc\" included substring that matched regex a(b*)c and group bb"))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught4 = intercept[TestFailedException] {
        "123abbcc" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some("\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught5 = intercept[TestFailedException] {
        "abbccdef" shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught5.message === Some("\"abbccdef\" included substring that matched regex a(b*)c and group bb"))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught6 = intercept[TestFailedException] {
        "abbccdef" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some("\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught7 = intercept[TestFailedException] {
        "123abbccdef" shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught7.message === Some("\"123abbccdef\" included substring that matched regex a(b*)c and group bb"))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught8 = intercept[TestFailedException] {
        "123abbccdef" shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some("\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught9 = intercept[TestFailedException] {
        "abbcc" shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught9.message === Some("\"abbcc\" included substring that matched regex a(b*)c and group bb"))
      assert(caught9.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught10 = intercept[TestFailedException] {
        "abbcc" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some("\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught10.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught11 = intercept[TestFailedException] {
        "123abbcc" shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught11.message === Some("\"123abbcc\" included substring that matched regex a(b*)c and group bb"))
      assert(caught11.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught12 = intercept[TestFailedException] {
        "123abbcc" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some("\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught12.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught13 = intercept[TestFailedException] {
        "abbccdef" shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught13.message === Some("\"abbccdef\" included substring that matched regex a(b*)c and group bb"))
      assert(caught13.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught14 = intercept[TestFailedException] {
        "abbccdef" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught14.message === Some("\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught14.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught15 = intercept[TestFailedException] {
        "123abbccdef" shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught15.message === Some("\"123abbccdef\" included substring that matched regex a(b*)c and group bb"))
      assert(caught15.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val caught16 = intercept[TestFailedException] {
        "123abbccdef" shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught16.message === Some("\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc"))
      assert(caught16.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
    }
    
    it("should work with contain key") {
      
      val map = Map("1" -> "one", "2" -> "two", "3" -> "three")
      
      map shouldNot contain key "7"
      
      val caught1 = intercept[TestFailedException] {
        map shouldNot contain key "1"
      }
      assert(caught1.message === Some(decorateToStringValue(prettifier, map) + " contained key \"1\""))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      
      
    }
    
    it("should work with contain value") {
      val map = Map("1" -> "one", "2" -> "two", "3" -> "three")
      
      map shouldNot contain value "seven"
      
      val caught1 = intercept[TestFailedException] {
        map shouldNot contain value "one"
      }
      assert(caught1.message === Some(decorateToStringValue(prettifier, map) + " contained value \"one\""))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
}
