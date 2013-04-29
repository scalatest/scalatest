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

import matchers.{BeMatcher, MatchResult, BePropertyMatcher, BePropertyMatchResult}

class ShouldBeShorthandSpec extends Spec with Matchers with SharedHelpers with EmptyMocks with BookPropertyMatchers {

  object `The shouldBe syntax` {

    def `should work with theSameInstanceAs` {
      val string = "Hi"
      val obj: AnyRef = string
      val otherString = new String("Hi")
      
      string shouldBe theSameInstanceAs (string)
      obj shouldBe theSameInstanceAs (string)
      string shouldBe theSameInstanceAs (obj)
      
      val caught1 = intercept[TestFailedException] {
        otherString shouldBe theSameInstanceAs (string)
      }
      assert(caught1.message === Some("\"Hi\" was not the same instance as \"Hi\""))
      assert(caught1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with any` {
      Array(1, 2) shouldBe Array(1, 2)
      1 shouldBe 1
      
      val caught1 = intercept[TestFailedException] {
        1 shouldBe 2
      }
      assert(caught1.getMessage === "1 was not equal to 2")
      assert(caught1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val s: String = null
      val caught2 = intercept[TestFailedException] {
        s shouldBe "hi"
      }
      assert(caught2.getMessage === "null was not equal to \"hi\"")
      assert(caught2.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      8 shouldBe > (7)
      val caught3 = intercept[TestFailedException] {
        7 shouldBe > (7)
      }
      assert(caught3.message === Some("7 was not greater than 7"))
      assert(caught3.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      5 shouldBe < (7) 
      val caught4 = intercept[TestFailedException] {
        7 shouldBe < (7)
      }
      assert(caught4.message === Some("7 was not less than 7"))
      assert(caught4.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      9 shouldBe >= (7)
      val caught5 = intercept[TestFailedException] {
        6 shouldBe >= (7)
      }
      assert(caught5.message === Some("6 was not greater than or equal to 7"))
      assert(caught5.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      3 shouldBe <= (7)
      val caught6 = intercept[TestFailedException] {
        8 shouldBe <= (7)
      }
      assert(caught6.message === Some("8 was not less than or equal to 7"))
      assert(caught6.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      true shouldBe true
      val caught7 = intercept[TestFailedException] {
        true shouldBe false
      }
      assert(caught7.message === Some("true was not equal to false"))
      assert(caught7.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with BeMatcher` {
      
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
      
      1 shouldBe odd
      2 shouldBe even
      
      val caught1 = intercept[TestFailedException] {
        4 shouldBe (odd)
      }
      assert(caught1.getMessage === "4 was even")
      assert(caught1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val caught2 = intercept[TestFailedException] {
        5 shouldBe (even)
      }
      assert(caught2.getMessage === "5 was odd")
      assert(caught2.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with symbol` {
      
      emptyMock shouldBe 'empty
      isEmptyMock shouldBe 'empty
      
      emptyMock shouldBe a ('empty)

      emptyMock shouldBe an ('empty)

      val ex1 = intercept[TestFailedException] {
        noPredicateMock shouldBe 'empty
      }
      assert(ex1.message === Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      assert(ex1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(ex1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      // Check message for name that starts with a consonant (should use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock shouldBe 'full
      }
      assert(ex2.message === Some("NoPredicateMock has neither a full nor an isFull method"))
      assert(ex2.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(ex2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val ex3 = intercept[TestFailedException] {
        noPredicateMock shouldBe a ('empty)
      }
      assert(ex3.message === Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      assert(ex3.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(ex3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val ex4 = intercept[TestFailedException] {
        noPredicateMock shouldBe an ('empty)
      }
      assert(ex4.message === Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      assert(ex4.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(ex4.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with BePropertyMatcher` {
      
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
      val badBook = new Book("A Tale of Two Cities", "Dickens", 1859, 45, false)
      
      book shouldBe goodRead
      
      val caught1 = intercept[TestFailedException] {
        badBook shouldBe goodRead
      }
      assert(caught1.message === Some("Book(A Tale of Two Cities,Dickens,1859,45,false) was not goodRead"))
      assert(caught1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      book shouldBe a (goodRead)
      
      val caught2 = intercept[TestFailedException] {
        badBook shouldBe a (goodRead)
      }
      assert(caught2.message === Some("Book(A Tale of Two Cities,Dickens,1859,45,false) was not a goodRead"))
      assert(caught2.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      book shouldBe an (goodRead)
      val caught3 = intercept[TestFailedException] {
        badBook shouldBe an (goodRead)
      }
      assert(caught3.message === Some("Book(A Tale of Two Cities,Dickens,1859,45,false) was not an goodRead"))
      assert(caught3.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    def `should with +-` {

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

      sevenDotOh shouldBe (7.1 +- 0.2)
      sevenDotOh shouldBe (6.9 +- 0.2)
      sevenDotOh shouldBe (7.0 +- 0.2)
      sevenDotOh shouldBe (7.2 +- 0.2)
      sevenDotOh shouldBe (6.8 +- 0.2)
      minusSevenDotOh shouldBe (-7.1 +- 0.2)
      minusSevenDotOh shouldBe (-6.9 +- 0.2)
      minusSevenDotOh shouldBe (-7.0 +- 0.2)
      minusSevenDotOh shouldBe (-7.2 +- 0.2)
      minusSevenDotOh shouldBe (-6.8 +- 0.2)

      // Double +- Float
      sevenDotOh shouldBe (7.1 +- 0.2f)
      sevenDotOh shouldBe (6.9 +- 0.2f)
      sevenDotOh shouldBe (7.0 +- 0.2f)
      sevenDotOh shouldBe (7.2 +- 0.2f)
      sevenDotOh shouldBe (6.8 +- 0.2f)
      minusSevenDotOh shouldBe (-7.1 +- 0.2f)
      minusSevenDotOh shouldBe (-6.9 +- 0.2f)
      minusSevenDotOh shouldBe (-7.0 +- 0.2f)
      minusSevenDotOh shouldBe (-7.2 +- 0.2f)
      minusSevenDotOh shouldBe (-6.8 +- 0.2f)

      // Double +- Long
      sevenDotOh shouldBe (7.1 +- 2L)
      sevenDotOh shouldBe (6.9 +- 2L)
      sevenDotOh shouldBe (7.0 +- 2L)
      sevenDotOh shouldBe (7.2 +- 2L)
      sevenDotOh shouldBe (6.8 +- 2L)
      minusSevenDotOh shouldBe (-7.1 +- 2L)
      minusSevenDotOh shouldBe (-6.9 +- 2L)
      minusSevenDotOh shouldBe (-7.0 +- 2L)
      minusSevenDotOh shouldBe (-7.2 +- 2L)
      minusSevenDotOh shouldBe (-6.8 +- 2L)

      // Double +- Int
      sevenDotOh shouldBe (7.1 +- 2)
      sevenDotOh shouldBe (6.9 +- 2)
      sevenDotOh shouldBe (7.0 +- 2)
      sevenDotOh shouldBe (7.2 +- 2)
      sevenDotOh shouldBe (6.8 +- 2)
      minusSevenDotOh shouldBe (-7.1 +- 2)
      minusSevenDotOh shouldBe (-6.9 +- 2)
      minusSevenDotOh shouldBe (-7.0 +- 2)
      minusSevenDotOh shouldBe (-7.2 +- 2)
      minusSevenDotOh shouldBe (-6.8 +- 2)

      // Double +- Short
      sevenDotOh shouldBe (7.1 +- 2.toShort)
      sevenDotOh shouldBe (6.9 +- 2.toShort)
      sevenDotOh shouldBe (7.0 +- 2.toShort)
      sevenDotOh shouldBe (7.2 +- 2.toShort)
      sevenDotOh shouldBe (6.8 +- 2.toShort)
      minusSevenDotOh shouldBe (-7.1 +- 2.toShort)
      minusSevenDotOh shouldBe (-6.9 +- 2.toShort)
      minusSevenDotOh shouldBe (-7.0 +- 2.toShort)
      minusSevenDotOh shouldBe (-7.2 +- 2.toShort)
      minusSevenDotOh shouldBe (-6.8 +- 2.toShort)

      // Double +- Byte
      sevenDotOh shouldBe (7.1 +- 2.toByte)
      sevenDotOh shouldBe (6.9 +- 2.toByte)
      sevenDotOh shouldBe (7.0 +- 2.toByte)
      sevenDotOh shouldBe (7.2 +- 2.toByte)
      sevenDotOh shouldBe (6.8 +- 2.toByte)
      minusSevenDotOh shouldBe (-7.1 +- 2.toByte)
      minusSevenDotOh shouldBe (-6.9 +- 2.toByte)
      minusSevenDotOh shouldBe (-7.0 +- 2.toByte)
      minusSevenDotOh shouldBe (-7.2 +- 2.toByte)
      minusSevenDotOh shouldBe (-6.8 +- 2.toByte)

      // Float +- Float
      sevenDotOhFloat shouldBe (7.1f +- 0.2f)
      sevenDotOhFloat shouldBe (6.9f +- 0.2f)
      sevenDotOhFloat shouldBe (7.0f +- 0.2f)
      sevenDotOhFloat shouldBe (7.2f +- 0.2f)
      sevenDotOhFloat shouldBe (6.8f +- 0.2f)
      minusSevenDotOhFloat shouldBe (-7.1f +- 0.2f)
      minusSevenDotOhFloat shouldBe (-6.9f +- 0.2f)
      minusSevenDotOhFloat shouldBe (-7.0f +- 0.2f)
      minusSevenDotOhFloat shouldBe (-7.2f +- 0.2f)
      minusSevenDotOhFloat shouldBe (-6.8f +- 0.2f)

      // Float +- Long
      sevenDotOhFloat shouldBe (7.1f +- 2L)
      sevenDotOhFloat shouldBe (6.9f +- 2L)
      sevenDotOhFloat shouldBe (7.0f +- 2L)
      sevenDotOhFloat shouldBe (7.2f +- 2L)
      sevenDotOhFloat shouldBe (6.8f +- 2L)
      minusSevenDotOhFloat shouldBe (-7.1f +- 2L)
      minusSevenDotOhFloat shouldBe (-6.9f +- 2L)
      minusSevenDotOhFloat shouldBe (-7.0f +- 2L)
      minusSevenDotOhFloat shouldBe (-7.2f +- 2L)
      minusSevenDotOhFloat shouldBe (-6.8f +- 2L)

      // Float +- Int
      sevenDotOhFloat shouldBe (7.1f +- 2)
      sevenDotOhFloat shouldBe (6.9f +- 2)
      sevenDotOhFloat shouldBe (7.0f +- 2)
      sevenDotOhFloat shouldBe (7.2f +- 2)
      sevenDotOhFloat shouldBe (6.8f +- 2)
      minusSevenDotOhFloat shouldBe (-7.1f +- 2)
      minusSevenDotOhFloat shouldBe (-6.9f +- 2)
      minusSevenDotOhFloat shouldBe (-7.0f +- 2)
      minusSevenDotOhFloat shouldBe (-7.2f +- 2)
      minusSevenDotOhFloat shouldBe (-6.8f +- 2)

      // Float +- Short
      sevenDotOhFloat shouldBe (7.1f +- 2.toShort)
      sevenDotOhFloat shouldBe (6.9f +- 2.toShort)
      sevenDotOhFloat shouldBe (7.0f +- 2.toShort)
      sevenDotOhFloat shouldBe (7.2f +- 2.toShort)
      sevenDotOhFloat shouldBe (6.8f +- 2.toShort)
      minusSevenDotOhFloat shouldBe (-7.1f +- 2.toShort)
      minusSevenDotOhFloat shouldBe (-6.9f +- 2.toShort)
      minusSevenDotOhFloat shouldBe (-7.0f +- 2.toShort)
      minusSevenDotOhFloat shouldBe (-7.2f +- 2.toShort)
      minusSevenDotOhFloat shouldBe (-6.8f +- 2.toShort)

      // Float +- Byte
      sevenDotOhFloat shouldBe (7.1f +- 2.toByte)
      sevenDotOhFloat shouldBe (6.9f +- 2.toByte)
      sevenDotOhFloat shouldBe (7.0f +- 2.toByte)
      sevenDotOhFloat shouldBe (7.2f +- 2.toByte)
      sevenDotOhFloat shouldBe (6.8f +- 2.toByte)
      minusSevenDotOhFloat shouldBe (-7.1f +- 2.toByte)
      minusSevenDotOhFloat shouldBe (-6.9f +- 2.toByte)
      minusSevenDotOhFloat shouldBe (-7.0f +- 2.toByte)
      minusSevenDotOhFloat shouldBe (-7.2f +- 2.toByte)
      minusSevenDotOhFloat shouldBe (-6.8f +- 2.toByte)

      // Long +- Long
      sevenLong shouldBe (9L +- 2L)
      sevenLong shouldBe (8L +- 2L)
      sevenLong shouldBe (7L +- 2L)
      sevenLong shouldBe (6L +- 2L)
      sevenLong shouldBe (5L +- 2L)
      minusSevenLong shouldBe (-9L +- 2L)
      minusSevenLong shouldBe (-8L +- 2L)
      minusSevenLong shouldBe (-7L +- 2L)
      minusSevenLong shouldBe (-6L +- 2L)
      minusSevenLong shouldBe (-5L +- 2L)

      // Long +- Int
      sevenLong shouldBe (9L +- 2)
      sevenLong shouldBe (8L +- 2)
      sevenLong shouldBe (7L +- 2)
      sevenLong shouldBe (6L +- 2)
      sevenLong shouldBe (5L +- 2)
      minusSevenLong shouldBe (-9L +- 2)
      minusSevenLong shouldBe (-8L +- 2)
      minusSevenLong shouldBe (-7L +- 2)
      minusSevenLong shouldBe (-6L +- 2)
      minusSevenLong shouldBe (-5L +- 2)

      // Long +- Short
      sevenLong shouldBe (9L +- 2.toShort)
      sevenLong shouldBe (8L +- 2.toShort)
      sevenLong shouldBe (7L +- 2.toShort)
      sevenLong shouldBe (6L +- 2.toShort)
      sevenLong shouldBe (5L +- 2.toShort)
      minusSevenLong shouldBe (-9L +- 2.toShort)
      minusSevenLong shouldBe (-8L +- 2.toShort)
      minusSevenLong shouldBe (-7L +- 2.toShort)
      minusSevenLong shouldBe (-6L +- 2.toShort)
      minusSevenLong shouldBe (-5L +- 2.toShort)

      // Long +- Byte
      sevenLong shouldBe (9L +- 2.toByte)
      sevenLong shouldBe (8L +- 2.toByte)
      sevenLong shouldBe (7L +- 2.toByte)
      sevenLong shouldBe (6L +- 2.toByte)
      sevenLong shouldBe (5L +- 2.toByte)
      minusSevenLong shouldBe (-9L +- 2.toByte)
      minusSevenLong shouldBe (-8L +- 2.toByte)
      minusSevenLong shouldBe (-7L +- 2.toByte)
      minusSevenLong shouldBe (-6L +- 2.toByte)
      minusSevenLong shouldBe (-5L +- 2.toByte)

      // Int +- Int
      sevenInt shouldBe (9 +- 2)
      sevenInt shouldBe (8 +- 2)
      sevenInt shouldBe (7 +- 2)
      sevenInt shouldBe (6 +- 2)
      sevenInt shouldBe (5 +- 2)
      minusSevenInt shouldBe (-9 +- 2)
      minusSevenInt shouldBe (-8 +- 2)
      minusSevenInt shouldBe (-7 +- 2)
      minusSevenInt shouldBe (-6 +- 2)
      minusSevenInt shouldBe (-5 +- 2)

      // Int +- Short
      sevenInt shouldBe (9 +- 2.toShort)
      sevenInt shouldBe (8 +- 2.toShort)
      sevenInt shouldBe (7 +- 2.toShort)
      sevenInt shouldBe (6 +- 2.toShort)
      sevenInt shouldBe (5 +- 2.toShort)
      minusSevenInt shouldBe (-9 +- 2.toShort)
      minusSevenInt shouldBe (-8 +- 2.toShort)
      minusSevenInt shouldBe (-7 +- 2.toShort)
      minusSevenInt shouldBe (-6 +- 2.toShort)
      minusSevenInt shouldBe (-5 +- 2.toShort)

      // Int +- Byte
      sevenInt shouldBe (9 +- 2.toByte)
      sevenInt shouldBe (8 +- 2.toByte)
      sevenInt shouldBe (7 +- 2.toByte)
      sevenInt shouldBe (6 +- 2.toByte)
      sevenInt shouldBe (5 +- 2.toByte)
      minusSevenInt shouldBe (-9 +- 2.toByte)
      minusSevenInt shouldBe (-8 +- 2.toByte)
      minusSevenInt shouldBe (-7 +- 2.toByte)
      minusSevenInt shouldBe (-6 +- 2.toByte)
      minusSevenInt shouldBe (-5 +- 2.toByte)

      // Short +- Short
      sevenShort shouldBe (9.toShort +- 2.toShort)
      sevenShort shouldBe (8.toShort +- 2.toShort)
      sevenShort shouldBe (7.toShort +- 2.toShort)
      sevenShort shouldBe (6.toShort +- 2.toShort)
      sevenShort shouldBe (5.toShort +- 2.toShort)
      minusSevenShort shouldBe ((-9).toShort +- 2.toShort)
      minusSevenShort shouldBe ((-8).toShort +- 2.toShort)
      minusSevenShort shouldBe ((-7).toShort +- 2.toShort)
      minusSevenShort shouldBe ((-6).toShort +- 2.toShort)
      minusSevenShort shouldBe ((-5).toShort +- 2.toShort)

      // Short +- Byte
      sevenShort shouldBe (9.toShort +- 2.toByte)
      sevenShort shouldBe (8.toShort +- 2.toByte)
      sevenShort shouldBe (7.toShort +- 2.toByte)
      sevenShort shouldBe (6.toShort +- 2.toByte)
      sevenShort shouldBe (5.toShort +- 2.toByte)
      minusSevenShort shouldBe ((-9).toShort +- 2.toByte)
      minusSevenShort shouldBe ((-8).toShort +- 2.toByte)
      minusSevenShort shouldBe ((-7).toShort +- 2.toByte)
      minusSevenShort shouldBe ((-6).toShort +- 2.toByte)
      minusSevenShort shouldBe ((-5).toShort +- 2.toByte)

      // Byte +- Byte
      sevenByte shouldBe (9.toByte +- 2.toByte)
      sevenByte shouldBe (8.toByte +- 2.toByte)
      sevenByte shouldBe (7.toByte +- 2.toByte)
      sevenByte shouldBe (6.toByte +- 2.toByte)
      sevenByte shouldBe (5.toByte +- 2.toByte)
      minusSevenByte shouldBe ((-9).toByte +- 2.toByte)
      minusSevenByte shouldBe ((-8).toByte +- 2.toByte)
      minusSevenByte shouldBe ((-7).toByte +- 2.toByte)
      minusSevenByte shouldBe ((-6).toByte +- 2.toByte)
      minusSevenByte shouldBe ((-5).toByte +- 2.toByte)

      val caught1 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 0.2)
      }
      assert(caught1.message === Some("7.0 was not 17.1 plus or minus 0.2"))
      assert(caught1.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Float
      val caught2 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 0.2f)
      }
      assert(caught2.message === Some("7.0 was not 17.1 plus or minus 0.20000000298023224"))
      assert(caught2.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Long
      val caught3 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 2L)
      }
      assert(caught3.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught3.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Int
      val caught4 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 2)
      }
      assert(caught4.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught4.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Short
      val caught5 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 2.toShort)
      }
      assert(caught5.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught5.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Byte
      val caught6 = intercept[TestFailedException] {
        sevenDotOh shouldBe (17.1 +- 2.toByte)
      }
      assert(caught6.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught6.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Float
      val caught7 = intercept[TestFailedException] {
        sevenDotOhFloat shouldBe (17.1f +- 0.2f)
      }
      assert(caught7.message === Some("7.0 was not 17.1 plus or minus 0.2"))
      assert(caught7.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Long
      val caught8 = intercept[TestFailedException] {
        sevenDotOhFloat shouldBe (17.1f +- 2L)
      }
      assert(caught8.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught8.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Int
      val caught9 = intercept[TestFailedException] {
        sevenDotOhFloat shouldBe (17.1f +- 2)
      }
      assert(caught9.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught9.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Short
      val caught10 = intercept[TestFailedException] {
        sevenDotOhFloat shouldBe (17.1f +- 2.toShort)
      }
      assert(caught10.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught10.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Byte
      val caught11 = intercept[TestFailedException] {
        sevenDotOhFloat shouldBe (17.1f +- 2.toByte)
      }
      assert(caught11.message === Some("7.0 was not 17.1 plus or minus 2.0"))
      assert(caught11.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Long
      val caught12 = intercept[TestFailedException] {
        sevenLong shouldBe (19L +- 2L)
      }
      assert(caught12.message === Some("7 was not 19 plus or minus 2"))
      assert(caught12.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Int
      val caught13 = intercept[TestFailedException] {
        sevenLong shouldBe (19L +- 2)
      }
      assert(caught13.message === Some("7 was not 19 plus or minus 2"))
      assert(caught13.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Short
      val caught14 = intercept[TestFailedException] {
        sevenLong shouldBe (19L +- 2.toShort)
      }
      assert(caught14.message === Some("7 was not 19 plus or minus 2"))
      assert(caught14.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Byte
      val caught15 = intercept[TestFailedException] {
        sevenLong shouldBe (19L +- 2.toByte)
      }
      assert(caught15.message === Some("7 was not 19 plus or minus 2"))
      assert(caught15.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Int
      val caught16 = intercept[TestFailedException] {
        sevenInt shouldBe (19 +- 2)
      }
      assert(caught16.message === Some("7 was not 19 plus or minus 2"))
      assert(caught16.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Short
      val caught17 = intercept[TestFailedException] {
        sevenInt shouldBe (19 +- 2.toShort)
      }
      assert(caught17.message === Some("7 was not 19 plus or minus 2"))
      assert(caught17.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Byte
      val caught18 = intercept[TestFailedException] {
        sevenInt shouldBe (19 +- 2.toByte)
      }
      assert(caught18.message === Some("7 was not 19 plus or minus 2"))
      assert(caught18.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Short
      val caught19 = intercept[TestFailedException] {
        sevenShort shouldBe (19.toShort +- 2.toShort)
      }
      assert(caught19.message === Some("7 was not 19 plus or minus 2"))
      assert(caught19.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Byte
      val caught20 = intercept[TestFailedException] {
        sevenShort shouldBe (19.toShort +- 2.toByte)
      }
      assert(caught20.message === Some("7 was not 19 plus or minus 2"))
      assert(caught20.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Byte +- Byte
      val caught21 = intercept[TestFailedException] {
        sevenByte shouldBe (19.toByte +- 2.toByte)
      }
      assert(caught21.message === Some("7 was not 19 plus or minus 2"))
      assert(caught21.failedCodeFileName === Some("ShouldBeShorthandSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

  }
}
