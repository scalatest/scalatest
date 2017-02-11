/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.anyvals

import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop._
import OptionValues._
import java.nio.charset.Charset
import java.util.Locale

import scala.util.{Failure, Success, Try}

class NumericStringSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import prop._

  implicit val numericStringGen: Generator[NumericString] =
    for (cs <- lists[Char](specificValues('0', '1', '2', '3', '4', '5', '6', '7', '8', '9'))) yield {
      if (cs.isEmpty) NumericString("000")
      else NumericString.ensuringValid(cs.mkString)
    }

  describe("A NumericString") {

    describe("should offer a from factory method that") {
      it("returns Some[NumericString] if the passed String contains only numeric characters") {
        NumericString.from("50").value.value shouldBe "50"
        NumericString.from("100").value.value shouldBe "100"
      }

      it("returns None if the passed String includes a non-numeric character") {
        NumericString.from("zero") shouldBe None
        NumericString.from("-1") shouldBe None
        NumericString.from("-99") shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns a NumericString if the passed String contains only numeric characters") {
        NumericString.ensuringValid("50").value shouldBe "50"
        NumericString.ensuringValid("100").value shouldBe "100"
      }

      it("throws AssertionError if the passed String includes a non-numeric character") {
        an [AssertionError] should be thrownBy NumericString.ensuringValid("zero")
        an [AssertionError] should be thrownBy NumericString.ensuringValid("-1")
        an [AssertionError] should be thrownBy NumericString.ensuringValid("-99")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed String contains only numeric characters") {
        NumericString.isValid("50") shouldBe true
        NumericString.isValid("100") shouldBe true
        NumericString.isValid("zero") shouldBe false
        NumericString.isValid("-1") shouldBe false
        NumericString.isValid("-99") shouldBe false
      }
    } 
    describe("should offer a fromOrElse factory method that") {
      it("returns a PosInt if the passed Int is greater than 0") {
        NumericString.fromOrElse("50", NumericString("42")).value shouldBe "50"
        NumericString.fromOrElse("100", NumericString("42")).value shouldBe "100"
      }
      it("returns a given default if the passed Int is NOT greater than 0") {
        NumericString.fromOrElse("zero", NumericString("42")).value shouldBe "42"
        NumericString.fromOrElse("-1", NumericString("42")).value shouldBe "42"
        NumericString.fromOrElse("-99", NumericString("42")).value shouldBe "42"
      }
    } 
    it("should offer an ensuringValid method that takes a String => String, throwing AssertionError if the result is invalid") {
      NumericString("33").ensuringValid(s => (s.toInt + 1).toString) shouldEqual NumericString("34")
      an [AssertionError] should be thrownBy { NumericString("33").ensuringValid(_ + "!") }
    }
    it("should offer a length method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.length shouldEqual (numStr.value.length)
      }
    }
    it("should offer a charAt method that is consistent with String") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val idx = pint % numStr.length
          numStr.charAt(idx) shouldEqual numStr.value.charAt(idx)
        }
      }
    }
    it("should offer a codePointAt method that is consistent with String") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val idx = pint % numStr.length
          numStr.codePointAt(idx) shouldEqual numStr.value.codePointAt(idx)
        }
      }
    }
    // SKIP-SCALATESTJS-START
    it("should offer a codePointBefore method that is consistent with String") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val idx = (pint % numStr.length) + 1

          numStr.codePointBefore(idx) shouldEqual
            numStr.value.codePointBefore(idx)
        }
      }
    }
    // SKIP-SCALATESTJS-END
    it("should offer a codePointCount method that is consistent with String") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2)

          numStr.codePointCount(beginIndex, endIndex) shouldEqual
            numStr.value.codePointCount(beginIndex, endIndex)
        }
      }
    }
    it("should offer a compareTo method that is consistent with String") {
      forAll { (numStr: NumericString, anotherString: String) =>
        numStr.compareTo(anotherString) shouldEqual
          numStr.value.compareTo(anotherString)

        val sameStr = new String(numStr.value)

        numStr.compareTo(sameStr) shouldEqual
          numStr.value.compareTo(sameStr)
      }
    }
    it("should offer a compareToIgnoreCase method that is consistent with String") {
      forAll { (numStr: NumericString, anotherString: String) =>
        numStr.compareToIgnoreCase(anotherString) shouldEqual
          numStr.value.compareToIgnoreCase(anotherString)

        val sameStr = new String(numStr.value)

        numStr.compareToIgnoreCase(sameStr) shouldEqual
          numStr.value.compareToIgnoreCase(sameStr)
      }
    }
    it("should offer a concat method that is consistent with String") {
      forAll { (numStr: NumericString, str: String) =>
        numStr.concat(str) shouldEqual
          numStr.value.concat(str)
      }
    }
    it("should offer a contains method that is consistent with String") {
      forAll { (numStr: NumericString, s: String, p1: PosInt, p2: PosInt) =>
        numStr.contains(s) shouldEqual
          numStr.value.contains(s)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val substr = numStr.value.substring(beginIndex, endIndex)

          numStr.contains(substr) shouldEqual
            numStr.value.contains(substr)
        }
      }
    }
    // SKIP-SCALATESTJS-START
    it("should offer a contentEquals method that is consistent with String") {
      forAll { (numStr: NumericString, str: String) =>
        val cs: CharSequence = str
        val matchingCs: CharSequence = numStr.value

        numStr.contentEquals(cs) shouldEqual
          numStr.value.contentEquals(cs)

        numStr.contentEquals(matchingCs) shouldEqual
          numStr.value.contentEquals(matchingCs)
      }
    }
    // SKIP-SCALATESTJS-END
    it("should offer an endsWith method that is consistent with String") {
      forAll { (numStr: NumericString, str: String, p1: PosInt) =>
        numStr.endsWith(str) shouldEqual
          numStr.value.endsWith(str)

        whenever (numStr.length > 0) {
          val idx = p1 % numStr.length
          val endStr = numStr.substring(idx)

          numStr.endsWith(str) shouldEqual
            numStr.value.endsWith(str)
        }
      }
    }
    it("should offer an equals method that is consistent with String") {
      forAll { (numStr: NumericString, numStr2: NumericString) =>
        numStr.equals(numStr) shouldEqual
          numStr.value.equals(numStr.value)

        numStr.equals(numStr2) shouldEqual
          numStr.value.equals(numStr2.value)
      }
    }
    it("should offer a getBytes method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.getBytes shouldEqual
          numStr.value.getBytes
      }
      forAll { (numStr: NumericString) =>
        numStr.getBytes(Charset.defaultCharset) shouldEqual
          numStr.value.getBytes(Charset.defaultCharset)
      }
      forAll { (numStr: NumericString) =>
        numStr.getBytes("UTF-16") shouldEqual
          numStr.value.getBytes("UTF-16")
      }
    }
    it("should offer a getChars method that is consistent with String") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val srcBegin = math.min(idx1, idx2)
          val srcEnd   = math.max(idx1, idx2) + 1

          // restrict size of dest array to avoid exceeding memory capacity
          val dstBegin = math.min(p3, 10 * numStr.length)
          val dstSize = dstBegin + numStr.length

          val dst1 = Array.fill[Char](dstSize)('-')
          val dst2 = Array.fill[Char](dstSize)('-')

          numStr.getChars(srcBegin, srcEnd, dst1, dstBegin)
          numStr.value.getChars(srcBegin, srcEnd, dst2, dstBegin)

          dst1 shouldEqual dst2
        }
      }
    }
    it("should offer an indexOf method that is consistent with String") {
      forAll { (numStr: NumericString, i1: Int, p1: PosInt) =>
        numStr.indexOf(i1) shouldEqual
          numStr.value.indexOf(i1)

        whenever (numStr.length > 0) {
          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.indexOf(findableCh) shouldEqual
            numStr.value.indexOf(findableCh)
        }
      }
      forAll { (numStr: NumericString, ch: Int, p1: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p1 % numStr.length

          numStr.indexOf(ch, fromIndex) shouldEqual
            numStr.value.indexOf(ch, fromIndex)

          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.indexOf(findableCh, fromIndex) shouldEqual
            numStr.value.indexOf(findableCh, fromIndex)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        numStr.indexOf(str) shouldEqual
          numStr.value.indexOf(str)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = numStr.substring(beginIndex, endIndex)

          numStr.indexOf(findableStr) shouldEqual
            numStr.value.indexOf(findableStr)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p3 % numStr.length

          numStr.indexOf(str, fromIndex) shouldEqual
            numStr.value.indexOf(str, fromIndex)

          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = numStr.substring(beginIndex, endIndex)

          numStr.indexOf(findableStr, fromIndex) shouldEqual
            numStr.value.indexOf(findableStr, fromIndex)
        }
      }
    }
    it("should offer a intern method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.intern shouldEqual
          numStr.value.intern
      }
    }
    it("should offer an isEmpty method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.isEmpty shouldEqual
          numStr.value.isEmpty
      }
    }
    it("should offer a lastIndexOf method that is consistent with String") {
      forAll { (numStr: NumericString, ch: Int, p1: PosInt) =>
        numStr.lastIndexOf(ch) shouldEqual
          numStr.value.lastIndexOf(ch)

        whenever (numStr.length > 0) {
          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.lastIndexOf(findableCh) shouldEqual
            numStr.value.lastIndexOf(findableCh)
        }
      }
      forAll { (numStr: NumericString, ch: Int, p1: PosInt, p2: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p2 % numStr.length

          numStr.lastIndexOf(ch, fromIndex) shouldEqual
            numStr.value.lastIndexOf(ch, fromIndex)

          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.lastIndexOf(findableCh, fromIndex) shouldEqual
            numStr.value.lastIndexOf(findableCh, fromIndex)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        numStr.lastIndexOf(str) shouldEqual
          numStr.value.lastIndexOf(str)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = numStr.substring(beginIndex, endIndex)

          numStr.lastIndexOf(findableStr) shouldEqual
            numStr.value.lastIndexOf(findableStr)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p3 % numStr.length

          numStr.lastIndexOf(str, fromIndex) shouldEqual
            numStr.value.lastIndexOf(str, fromIndex)

          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = numStr.substring(beginIndex, endIndex)

          numStr.lastIndexOf(findableStr, fromIndex) shouldEqual
            numStr.value.lastIndexOf(findableStr, fromIndex)
        }
      }
    }
    it("should offer a matches method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        val r1 = """[0-9]+"""
        val r2 = """[a-z]+"""

        numStr.matches(r1) shouldEqual
          numStr.value.matches(r1)
        numStr.matches(r2) shouldEqual
          numStr.value.matches(r2)
      }
    }
    // SKIP-SCALATESTJS-START
    it("should offer a offsetByCodePoints method that is consistent with String") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        whenever (numStr.length > 0) {
          val index = p1 % numStr.length
          val codePointOffset = (p2 % numStr.length) - index

          numStr.offsetByCodePoints(index, codePointOffset) shouldEqual
            numStr.value.offsetByCodePoints(index, codePointOffset)
        }
      }
    }
    // SKIP-SCALATESTJS-END
    it("should offer regionMatches methods that are consistent with String") {
      forAll { (numStr: NumericString, i1: Int, str: String, i2: Int, len: Int ) =>
        numStr.regionMatches(true, i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(true, i1, str, i2, len)

        numStr.regionMatches(false, i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(false, i1, str, i2, len)

        numStr.regionMatches(i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(true, i1, str, i2, len)

        whenever (numStr.length > 0) { // test a true condition
          val p1 = math.abs(i1/2) // /2 to avoid abs(Int.MinValue) problem
          val p2 = math.abs(i2/2)

          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val offset = math.min(idx1, idx2)
          val len = math.max(idx1, idx2) - offset + 1
          val sameStr = new String(numStr.value)

          numStr.regionMatches(false, offset, sameStr, offset, len) shouldEqual
            numStr.value.regionMatches(false, offset, sameStr, offset, len)

          numStr.regionMatches(offset, sameStr, offset, len) shouldEqual
            numStr.value.regionMatches(offset, sameStr, offset, len)
        }
      }
    }
    it("should offer replace methods that are consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.replace('0', '1') shouldEqual
          numStr.value.replace('0', '1')
        numStr.replace('9', '1') shouldEqual
          numStr.value.replace('9', '1')
        numStr.replace('a', '1') shouldEqual
          numStr.value.replace('a', '1')
        numStr.replace("0", "1") shouldEqual
          numStr.value.replace("0", "1")
        numStr.replace("0", "123") shouldEqual
          numStr.value.replace("0", "123")
        numStr.replace("0123", "4") shouldEqual
          numStr.value.replace("0123", "4")
        numStr.replace("", "4") shouldEqual
          numStr.value.replace("", "4")
      }
    }
    it("should offer a replaceAll method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.replaceAll("0+", "1")shouldEqual
          numStr.value.replaceAll("0+", "1")
      }
    }
    it("should offer a replaceFirst method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.replaceFirst("0+", "1")shouldEqual
          numStr.value.replaceFirst("0+", "1")
      }
    }
    it("should offer split methods that are consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.split("0+") shouldEqual
          numStr.value.split("0+")
        numStr.split("0+", -1) shouldEqual
          numStr.value.split("0+", -1)
        numStr.split("0+", 0) shouldEqual
          numStr.value.split("0+", 0)
        numStr.split("0+", 1) shouldEqual
          numStr.value.split("0+", 1)
        numStr.split("0+", 2) shouldEqual
          numStr.value.split("0+", 2)
      }
    }
    it("should offer startsWith methods that are consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.startsWith("0") shouldEqual
          numStr.value.startsWith("0")
        numStr.startsWith("20") shouldEqual
          numStr.value.startsWith("20")

        numStr.startsWith("0", 0) shouldEqual
          numStr.value.startsWith("0", 0)
        numStr.startsWith("20", 20) shouldEqual
          numStr.value.startsWith("20", 20)
        numStr.startsWith("20", -20) shouldEqual
          numStr.value.startsWith("20", -20)
      }
    }
    it("should offer a subSequence method that is consistent with String") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          numStr.subSequence(beginIndex, endIndex) shouldEqual
            numStr.value.subSequence(beginIndex, endIndex)
        }
      }
    }
    it("should offer substring methods that are consistent with String") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          numStr.substring(beginIndex) shouldEqual
            numStr.value.substring(beginIndex)

          numStr.substring(beginIndex, endIndex) shouldEqual
            numStr.value.substring(beginIndex, endIndex)
        }
      }
    }
    it("should offer a toCharArray method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.toCharArray shouldEqual
          numStr.value.toCharArray
      }
    }
    // SKIP-SCALATESTJS-START
    it("should offer toLowerCase methods that are consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.toLowerCase shouldEqual
          numStr.value.toLowerCase

        numStr.toLowerCase(Locale.getDefault) shouldEqual
          numStr.value.toLowerCase(Locale.getDefault)
      }
    }
    it("should offer toUpperCase methods that are consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.toUpperCase shouldEqual
          numStr.value.toUpperCase

        numStr.toUpperCase(Locale.getDefault) shouldEqual
          numStr.value.toUpperCase(Locale.getDefault)
      }
    }
    // SKIP-SCALATESTJS-END
    it("should offer a trim method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.trim shouldEqual
          numStr.value.trim
      }
    }
  }
}

