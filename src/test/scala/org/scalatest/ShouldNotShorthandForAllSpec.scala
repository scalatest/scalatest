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
import SharedHelpers._
import FailureMessages.decorateToStringValue

class ShouldNotShorthandForAllSpec extends Spec with Matchers with EmptyMocks with BookPropertyMatchers {
  
  def errorMessage(index: Int, message: String, lineNumber: Int, left: Any): String = 
    "'all' inspection failed, because: \n" +
    "  at index " + index + ", " + message + " (ShouldNotShorthandForAllSpec.scala:" + lineNumber + ") \n" +
    "in " + decorateToStringValue(left)
  
  object `The shouldNot syntax` {
    
    def `should work with theSameInstanceAs` {
      
      val string = "Hi"
      val obj: AnyRef = string
      val otherString = new String("Hi")
      
      all(List(otherString)) shouldNot { be theSameInstanceAs (string) }
      all(List(otherString)) shouldNot be theSameInstanceAs (string)
      
      val list1 = List(obj)
      val caught1 = intercept[TestFailedException] {
        all(list1) shouldNot { be theSameInstanceAs (string) }
      }
      assert(caught1.message === Some(errorMessage(0, "\"Hi\" was the same instance as \"Hi\"", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List(obj)
      val caught2 = intercept[TestFailedException] {
        all(list2) shouldNot be theSameInstanceAs string
      }
      assert(caught2.message === Some(errorMessage(0, "\"Hi\" was the same instance as \"Hi\"", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with any` {
      all(List(1)) shouldNot { be (2) }
      all(List(1)) shouldNot be (2)
      all(List("hi")) shouldNot be (null)
      
      val list1 = List(1)
      val caught1 = intercept[TestFailedException] {
        all(list1) shouldNot { be (1) }
      }
      assert(caught1.message === Some(errorMessage(0, "1 was equal to 1", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List(1)
      val caught2 = intercept[TestFailedException] {
        all(list1) shouldNot be (1)
      }
      assert(caught2.message === Some(errorMessage(0, "1 was equal to 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List[String](null)
      val caught3 = intercept[TestFailedException] {
        all(list3) shouldNot be (null)
      }
      assert(caught3.message === Some(errorMessage(0, "The reference was null", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List(1)
      val caught4 = intercept[TestFailedException] {
        all(list4) shouldNot be (1)
      }
      assert(caught4.message === Some(errorMessage(0, "1 was equal to 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List(1)
      val caught5 = intercept[TestFailedException] {
        all(list5) shouldNot (not (not be (1)))
      }
      assert(caught5.message === Some(errorMessage(0, "1 was equal to 1", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      all(List(6)) shouldNot be > 7
      val list6 = List(8)
      val caught6 = intercept[TestFailedException] {
        all(list6) shouldNot be > 7
      }
      assert(caught6.message === Some(errorMessage(0, "8 was greater than 7", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      all(List(8)) shouldNot be < 7
      val list7 = List(5)
      val caught7 = intercept[TestFailedException] {
        all(list7) shouldNot be < 7
      }
      assert(caught7.message === Some(errorMessage(0, "5 was less than 7", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      all(List(3)) shouldNot be >= 7
      val list8 = List(8)
      val caught8 = intercept[TestFailedException] {
        all(list8) shouldNot be >= 7
      }
      assert(caught8.message === Some(errorMessage(0, "8 was greater than or equal to 7", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      all(List(8)) shouldNot be <= 7
      val list9 = List(3)
      val caught9 = intercept[TestFailedException] {
        all(list9) shouldNot be <= 7
      }
      assert(caught9.message === Some(errorMessage(0, "3 was less than or equal to 7", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      all(List(true)) shouldNot be (false)
      val list10 = List(true)
      val caught10 = intercept[TestFailedException] {
        all(list10) shouldNot be (true)
      }
      assert(caught10.message === Some(errorMessage(0, "true was true", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      all(List(2)) shouldNot be (odd)
      all(List(1)) shouldNot be (even)
      all(List(22)) shouldNot (not (be (even)))
      all(List(1)) shouldNot (not (be (odd)))
      
      val list1 = List(3)
      val caught1 = intercept[TestFailedException] {
        all(list1) shouldNot be (odd)
      }
      assert(caught1.message === Some(errorMessage(0, "3 was odd", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List(6)
      val caught2 = intercept[TestFailedException] {
        all(list2) shouldNot be (even)
      }
      assert(caught2.message === Some(errorMessage(0, "6 was even", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List(6)
      val caught3 = intercept[TestFailedException] {
        all(list3) shouldNot (not (be (odd)))
      }
      assert(caught3.message === Some(errorMessage(0, "6 was even", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    def `should work with symbol` {
      
      all(List(notEmptyMock)) shouldNot { be ('empty) }
      all(List(notEmptyMock)) shouldNot be ('empty)
      all(List(isNotEmptyMock)) shouldNot { be ('empty) }
      all(List(isNotEmptyMock)) shouldNot be ('empty)
      
      val list1 = List(noPredicateMock)
      val ex1 = intercept[TestFailedException] {
        all(list1) shouldNot { be ('empty) }
      }
      assert(ex1.message === Some(errorMessage(0, "NoPredicateMock has neither an empty nor an isEmpty method", thisLineNumber - 2, list1)))
      assert(ex1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(ex1.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list2 = List(noPredicateMock)
      val ex2 = intercept[TestFailedException] {
        all(list2) shouldNot (be ('full))
      }
      assert(ex2.message === Some(errorMessage(0, "NoPredicateMock has neither a full nor an isFull method", thisLineNumber - 2, list2)))
      assert(ex2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(ex2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list3 = List(noPredicateMock)
      val ex3 = intercept[TestFailedException] {
        all(list3) shouldNot be ('empty)
      }
      assert(ex3.message === Some(errorMessage(0, "NoPredicateMock has neither an empty nor an isEmpty method", thisLineNumber - 2, list3)))
      assert(ex3.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(ex3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list4 = List(noPredicateMock)
      val ex4 = intercept[TestFailedException] {
        all(list4) shouldNot be ('full)
      }
      assert(ex4.message === Some(errorMessage(0, "NoPredicateMock has neither a full nor an isFull method", thisLineNumber - 2, list4)))
      assert(ex4.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
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
      
      all(List(badBook)) shouldNot be (goodRead)
      all(List(badBook)) shouldNot be a goodRead
      all(List(badBook)) shouldNot be an goodRead
      
      val list1 = List(book)
      val caught1 = intercept[TestFailedException] {
        all(list1) shouldNot be (goodRead)
      }
      assert(caught1.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was goodRead", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List(book)
      val caught2 = intercept[TestFailedException] {
        all(list2) shouldNot be a goodRead
      }
      assert(caught2.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was a goodRead", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list3 = List(book)
      val caught3 = intercept[TestFailedException] {
        all(list3) shouldNot be an goodRead
      }
      assert(caught3.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was an goodRead", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list4 = List(book)
      val caught4 = intercept[TestFailedException] {
        all(list4) shouldNot (be (goodRead))
      }
      assert(caught4.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was goodRead", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list5 = List(book)
      val caught5 = intercept[TestFailedException] {
        all(list5) shouldNot (be a (goodRead))
      }
      assert(caught5.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was a goodRead", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list6 = List(book)
      val caught6 = intercept[TestFailedException] {
        all(list6) shouldNot (be an (goodRead))
      }
      assert(caught6.message === Some(errorMessage(0, "Book(A Tale of Two Cities,Dickens,1859,45,true) was an goodRead", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      // Double +- Double
      all(List(sevenDotOh)) shouldNot { be (7.5 +- 0.2) }
      all(List(sevenDotOh)) shouldNot be (7.5 +- 0.2)
      all(List(sevenDotOh)) shouldNot be (6.5 +- 0.2)
      all(List(minusSevenDotOh)) shouldNot { be (-7.5 +- 0.2) }
      all(List(minusSevenDotOh)) shouldNot be (-7.5 +- 0.2)
      all(List(minusSevenDotOh)) shouldNot be (-6.5 +- 0.2)
      
      // Double +- Float
      all(List(sevenDotOh)) shouldNot { be (7.5 +- 0.2f) }
      all(List(sevenDotOh)) shouldNot be (7.5 +- 0.2f)
      all(List(sevenDotOh)) shouldNot be (6.5 +- 0.2f)
      all(List(minusSevenDotOh)) shouldNot { be (-7.5 +- 0.2f) }
      all(List(minusSevenDotOh)) shouldNot be (-7.5 +- 0.2f)
      all(List(minusSevenDotOh)) shouldNot be (-6.5 +- 0.2f)

      // Double +- Long
      all(List(sevenDotOh)) shouldNot { be (10.0 +- 2L) }
      all(List(sevenDotOh)) shouldNot be (4.0 +- 2L)
      all(List(sevenDotOh)) shouldNot be (9.1 +- 2L)
      all(List(minusSevenDotOh)) shouldNot { be (-10.0 +- 2L) }
      all(List(minusSevenDotOh)) shouldNot be (-4.0 +- 2L)
      all(List(minusSevenDotOh)) shouldNot be (-9.1 +- 2L)

      // Double +- Int
      all(List(sevenDotOh)) shouldNot { be (10.0 +- 2) }
      all(List(sevenDotOh)) shouldNot be (4.0 +- 2)
      all(List(sevenDotOh)) shouldNot be (9.1 +- 2)
      all(List(minusSevenDotOh)) shouldNot { be (-10.0 +- 2) }
      all(List(minusSevenDotOh)) shouldNot be (-4.0 +- 2)
      all(List(minusSevenDotOh)) shouldNot be (-9.1 +- 2)

      // Double +- Short
      all(List(sevenDotOh)) shouldNot { be (10.0 +- 2.toShort) }
      all(List(sevenDotOh)) shouldNot be (4.0 +- 2.toShort)
      all(List(sevenDotOh)) shouldNot be (9.1 +- 2.toShort)
      all(List(minusSevenDotOh)) shouldNot { be (-10.0 +- 2.toShort) }
      all(List(minusSevenDotOh)) shouldNot be (-4.0 +- 2.toShort)
      all(List(minusSevenDotOh)) shouldNot be (-9.1 +- 2.toShort)

      // Double +- Byte
      all(List(sevenDotOh)) shouldNot { be (10.0 +- 2.toByte) }
      all(List(sevenDotOh)) shouldNot be (4.0 +- 2.toByte)
      all(List(sevenDotOh)) shouldNot be (9.1 +- 2.toByte)
      all(List(minusSevenDotOh)) shouldNot { be (-10.0 +- 2.toByte) }
      all(List(minusSevenDotOh)) shouldNot be (-4.0 +- 2.toByte)
      all(List(minusSevenDotOh)) shouldNot be (-9.1 +- 2.toByte)

      // Float +- Float
      all(List(sevenDotOhFloat)) shouldNot { be (7.5f +- 0.2f) }
      all(List(sevenDotOhFloat)) shouldNot be (7.5f +- 0.2f)
      all(List(sevenDotOhFloat)) shouldNot be (6.5f +- 0.2f)
      all(List(minusSevenDotOhFloat)) shouldNot { be (-7.5f +- 0.2f) }
      all(List(minusSevenDotOhFloat)) shouldNot be (-7.5f +- 0.2f)
      all(List(minusSevenDotOhFloat)) shouldNot be (-6.5f +- 0.2f)

      // Float +- Long
      all(List(sevenDotOhFloat)) shouldNot { be (10.0f +- 2L) }
      all(List(sevenDotOhFloat)) shouldNot be (4.0f +- 2L)
      all(List(sevenDotOhFloat)) shouldNot be (9.1f +- 2L)
      all(List(minusSevenDotOhFloat)) shouldNot { be (-10.0f +- 2L) }
      all(List(minusSevenDotOhFloat)) shouldNot be (-4.0f +- 2L)
      all(List(minusSevenDotOhFloat)) shouldNot be (-9.1f +- 2L)

      // Float +- Int
      all(List(sevenDotOhFloat)) shouldNot { be (10.0f +- 2) }
      all(List(sevenDotOhFloat)) shouldNot be (4.0f +- 2)
      all(List(sevenDotOhFloat)) shouldNot be (9.1f +- 2)
      all(List(minusSevenDotOhFloat)) shouldNot { be (-10.0f +- 2) }
      all(List(minusSevenDotOhFloat)) shouldNot be (-4.0f +- 2)
      all(List(minusSevenDotOhFloat)) shouldNot be (-9.1f +- 2)

      // Float +- Short
      all(List(sevenDotOhFloat)) shouldNot { be (10.0f +- 2.toShort) }
      all(List(sevenDotOhFloat)) shouldNot be (4.0f +- 2.toShort)
      all(List(sevenDotOhFloat)) shouldNot be (9.1f +- 2.toShort)
      all(List(minusSevenDotOhFloat)) shouldNot { be (-10.0f +- 2.toShort) }
      all(List(minusSevenDotOhFloat)) shouldNot be (-4.0f +- 2.toShort)
      all(List(minusSevenDotOhFloat)) shouldNot be (-9.1f +- 2.toShort)

      // Float +- Byte
      all(List(sevenDotOhFloat)) shouldNot { be (10.0f +- 2.toByte) }
      all(List(sevenDotOhFloat)) shouldNot be (4.0f +- 2.toByte)
      all(List(sevenDotOhFloat)) shouldNot be (9.1f +- 2.toByte)
      all(List(minusSevenDotOhFloat)) shouldNot { be (-10.0f +- 2.toByte) }
      all(List(minusSevenDotOhFloat)) shouldNot be (-4.0f +- 2.toByte)
      all(List(minusSevenDotOhFloat)) shouldNot be (-9.1f +- 2.toByte)

      // Long +- Long
      all(List(sevenLong)) shouldNot { be (10L +- 2L) }
      all(List(sevenLong)) shouldNot be (4L +- 2L)
      all(List(sevenLong)) shouldNot be (10L +- 2L)
      all(List(minusSevenLong)) shouldNot { be (-10L +- 2L) }
      all(List(minusSevenLong)) shouldNot be (-4L +- 2L)
      all(List(minusSevenLong)) shouldNot be (-10L +- 2L)

      // Long +- Int
      all(List(sevenLong)) shouldNot { be (10L +- 2) }
      all(List(sevenLong)) shouldNot be (4L +- 2)
      all(List(sevenLong)) shouldNot be (10L +- 2)
      all(List(minusSevenLong)) shouldNot { be (-10L +- 2) }
      all(List(minusSevenLong)) shouldNot be (-4L +- 2)
      all(List(minusSevenLong)) shouldNot be (-10L +- 2)

      // Long +- Short
      all(List(sevenLong)) shouldNot { be (10L +- 2.toShort) }
      all(List(sevenLong)) shouldNot be (4L +- 2.toShort)
      all(List(sevenLong)) shouldNot be (10L +- 2.toShort)
      all(List(minusSevenLong)) shouldNot { be (-10L +- 2.toShort) }
      all(List(minusSevenLong)) shouldNot be (-4L +- 2.toShort)
      all(List(minusSevenLong)) shouldNot be (-10L +- 2.toShort)

      // Long +- Byte
      all(List(sevenLong)) shouldNot { be (10L +- 2.toByte) }
      all(List(sevenLong)) shouldNot be (4L +- 2.toByte)
      all(List(sevenLong)) shouldNot be (10L +- 2.toByte)
      all(List(minusSevenLong)) shouldNot { be (-10L +- 2.toByte) }
      all(List(minusSevenLong)) shouldNot be (-4L +- 2.toByte)
      all(List(minusSevenLong)) shouldNot be (-10L +- 2.toByte)

      // Int +- Int
      all(List(sevenInt)) shouldNot { be (10 +- 2) }
      all(List(sevenInt)) shouldNot be (4 +- 2)
      all(List(sevenInt)) shouldNot be (10 +- 2)
      all(List(minusSevenInt)) shouldNot { be (-10 +- 2) }
      all(List(minusSevenInt)) shouldNot be (-4 +- 2)
      all(List(minusSevenInt)) shouldNot be (-10 +- 2)

      // Int +- Short
      all(List(sevenInt)) shouldNot { be (10 +- 2.toShort) }
      all(List(sevenInt)) shouldNot be (4 +- 2.toShort)
      all(List(sevenInt)) shouldNot be (10 +- 2.toShort)
      all(List(minusSevenInt)) shouldNot { be (-10 +- 2.toShort) }
      all(List(minusSevenInt)) shouldNot be (-4 +- 2.toShort)
      all(List(minusSevenInt)) shouldNot be (-10 +- 2.toShort)

      // Int +- Byte
      all(List(sevenInt)) shouldNot { be (10 +- 2.toByte) }
      all(List(sevenInt)) shouldNot be (4 +- 2.toByte)
      all(List(sevenInt)) shouldNot be (10 +- 2.toByte)
      all(List(minusSevenInt)) shouldNot { be (-10 +- 2.toByte) }
      all(List(minusSevenInt)) shouldNot be (-4 +- 2.toByte)
      all(List(minusSevenInt)) shouldNot be (-10 +- 2.toByte)

      // Short +- Short
      all(List(sevenShort)) shouldNot { be (10.toShort +- 2.toShort) }
      all(List(sevenShort)) shouldNot be (4.toShort +- 2.toShort)
      all(List(sevenShort)) shouldNot be (10.toShort +- 2.toShort)
      all(List(minusSevenShort)) shouldNot { be ((-10).toShort +- 2.toShort) }
      all(List(minusSevenShort)) shouldNot be ((-4).toShort +- 2.toShort)
      all(List(minusSevenShort)) shouldNot be ((-10).toShort +- 2.toShort)

      // Short +- Byte
      all(List(sevenShort)) shouldNot { be (10.toShort +- 2.toByte) }
      all(List(sevenShort)) shouldNot be (4.toShort +- 2.toByte)
      all(List(sevenShort)) shouldNot be (10.toShort +- 2.toByte)
      all(List(minusSevenShort)) shouldNot { be ((-10).toShort +- 2.toByte) }
      all(List(minusSevenShort)) shouldNot be ((-4).toShort +- 2.toByte)
      all(List(minusSevenShort)) shouldNot be ((-10).toShort +- 2.toByte)

      // Byte +- Byte
      all(List(sevenByte)) shouldNot { be (10.toByte +- 2.toByte) }
      all(List(sevenByte)) shouldNot be (4.toByte +- 2.toByte)
      all(List(sevenByte)) shouldNot be (10.toByte +- 2.toByte)
      all(List(minusSevenByte)) shouldNot { be ((-10).toByte +- 2.toByte) }
      all(List(minusSevenByte)) shouldNot be ((-4).toByte +- 2.toByte)
      all(List(minusSevenByte)) shouldNot be ((-10).toByte +- 2.toByte)
      
      // Double +- Double
      val list1 = List(sevenDotOh)
      val caught1 = intercept[TestFailedException] {
        all(list1) shouldNot be (7.1 +- 0.2)
      }
      assert(caught1.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 0.2", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Float
      val list2 = List(sevenDotOh)
      val caught2 = intercept[TestFailedException] {
        all(list2) shouldNot be (7.1 +- 0.2f)
      }
      assert(caught2.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 0.20000000298023224", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Long
      val list3 = List(sevenDotOh)
      val caught3 = intercept[TestFailedException] {
        all(list3) shouldNot be (7.1 +- 2L)
      }
      assert(caught3.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Int
      val list4 = List(sevenDotOh)
      val caught4 = intercept[TestFailedException] {
        all(list4) shouldNot be (7.1 +- 2)
      }
      assert(caught4.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Short
      val list5 = List(sevenDotOh)
      val caught5 = intercept[TestFailedException] {
        all(list5) shouldNot be (7.1 +- 2.toShort)
      }
      assert(caught5.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Double +- Byte
      val list6 = List(sevenDotOh)
      val caught6 = intercept[TestFailedException] {
        all(list6) shouldNot be (7.1 +- 2.toByte)
      }
      assert(caught6.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Float
      val list7 = List(sevenDotOhFloat)
      val caught7 = intercept[TestFailedException] {
        all(list7) shouldNot be (7.1f +- 0.2f)
      }
      assert(caught7.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 0.2", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Long
      val list8 = List(sevenDotOhFloat)
      val caught8 = intercept[TestFailedException] {
        all(list8) shouldNot be (7.1f +- 2L)
      }
      assert(caught8.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Int
      val list9 = List(sevenDotOhFloat)
      val caught9 = intercept[TestFailedException] {
        all(list9) shouldNot be (7.1f +- 2)
      }
      assert(caught9.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Short
      val list10 = List(sevenDotOhFloat)
      val caught10 = intercept[TestFailedException] {
        all(list10) shouldNot be (7.1f +- 2.toShort)
      }
      assert(caught10.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Float +- Byte
      val list11 = List(sevenDotOhFloat)
      val caught11 = intercept[TestFailedException] {
        all(list11) shouldNot be (7.1f +- 2.toByte)
      }
      assert(caught11.message === Some(errorMessage(0, "7.0 was 7.1 plus or minus 2.0", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Long
      val list12 = List(sevenLong)
      val caught12 = intercept[TestFailedException] {
        all(list12) shouldNot be (9L +- 2L)
      }
      assert(caught12.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Int
      val list13 = List(sevenLong)
      val caught13 = intercept[TestFailedException] {
        all(list13) shouldNot be (9L +- 2)
      }
      assert(caught13.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Short
      val list14 = List(sevenLong)
      val caught14 = intercept[TestFailedException] {
        all(list14) shouldNot be (9L +- 2.toShort)
      }
      assert(caught14.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Long +- Byte
      val list15 = List(sevenLong)
      val caught15 = intercept[TestFailedException] {
        all(list15) shouldNot be (9L +- 2.toByte)
      }
      assert(caught15.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Int
      val list16 = List(sevenInt)
      val caught16 = intercept[TestFailedException] {
        all(list16) shouldNot be (9 +- 2)
      }
      assert(caught16.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Short
      val list17 = List(sevenInt)
      val caught17 = intercept[TestFailedException] {
        all(list17) shouldNot be (9 +- 2.toShort)
      }
      assert(caught17.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Int +- Byte
      val list18 = List(sevenInt)
      val caught18 = intercept[TestFailedException] {
        all(list18) shouldNot be (9 +- 2.toByte)
      }
      assert(caught18.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Short
      val list19 =  List(sevenShort)
      val caught19 = intercept[TestFailedException] {
        all(list19) shouldNot be (9.toShort +- 2.toShort)
      }
      assert(caught19.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Short +- Byte
      val list20 = List(sevenShort)
      val caught20 = intercept[TestFailedException] {
        all(list20) shouldNot be (9.toShort +- 2.toByte)
      }
      assert(caught20.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))

      // Byte +- Byte
      val list21 = List(sevenByte)
      val caught21 = intercept[TestFailedException] {
        all(list21) shouldNot be (9.toByte +- 2.toByte)
      }
      assert(caught21.message === Some(errorMessage(0, "7 was 9 plus or minus 2", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("ShouldNotShorthandForAllSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
}