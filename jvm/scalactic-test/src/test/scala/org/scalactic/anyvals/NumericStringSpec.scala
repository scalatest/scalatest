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
package org.scalactic.anyvals

import org.scalactic.Equality
import org.scalatest._
import org.scalatest.prop._
import OptionValues._
import java.nio.charset.Charset
import scala.collection.mutable.ArrayBuffer

import scala.util.{Failure, Success, Try}
import TryValues._
import org.scalactic.{Pass, Fail}
import org.scalactic.{Good, Bad}

import org.scalactic.ColCompatHelper.aggregate

class NumericStringSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks {

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
      it("returns a NumericString if the passed String is numeric") {
        NumericString.fromOrElse("50", NumericString("42")).value shouldBe "50"
        NumericString.fromOrElse("100", NumericString("42")).value shouldBe "100"
      }
      it("returns a given default if the passed String is NOT numeric") {
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
    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer a codePointBefore method that is consistent with String") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val idx = (pint % numStr.length) + 1

          numStr.codePointBefore(idx) shouldEqual
            numStr.value.codePointBefore(idx)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
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
/*
    Please leave this here as a template for other String AnyVals that will have this method.
    it("should offer a compareToIgnoreCase method that is consistent with String") {
      forAll { (numStr: NumericString, anotherString: String) =>
        numStr.compareToIgnoreCase(anotherString) shouldEqual
          numStr.value.compareToIgnoreCase(anotherString)

        val sameStr = new String(numStr.value)

        numStr.compareToIgnoreCase(sameStr) shouldEqual
          numStr.value.compareToIgnoreCase(sameStr)
      }
    }
*/
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
    // SKIP-SCALATESTJS,NATIVE-START
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
    // SKIP-SCALATESTJS,NATIVE-END
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
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6705
      forAll { (numStr: NumericString) =>
        numStr.getBytes shouldEqual
          numStr.value.getBytes
      }
      // SKIP-DOTTY-END
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
        //SCALATESTJS,NATIVE-ONLY try {
          numStr.indexOf(i1) shouldEqual
            numStr.value.indexOf(i1)

          whenever (numStr.length > 0) {
            val idx = p1 % numStr.length
            val findableCh = numStr.charAt(idx)

            numStr.indexOf(findableCh) shouldEqual
              numStr.value.indexOf(findableCh)
          }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, ch: Int, p1: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (numStr.length > 0) {
          val fromIndex = p1 % numStr.length

          numStr.indexOf(ch, fromIndex) shouldEqual
            numStr.value.indexOf(ch, fromIndex)

          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.indexOf(findableCh, fromIndex) shouldEqual
            numStr.value.indexOf(findableCh, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
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
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
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
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
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
        //SCALATESTJS,NATIVE-ONLY try {
          numStr.lastIndexOf(ch) shouldEqual
            numStr.value.lastIndexOf(ch)

          whenever (numStr.length > 0) {
            val idx = p1 % numStr.length
            val findableCh = numStr.charAt(idx)

            numStr.lastIndexOf(findableCh) shouldEqual
              numStr.value.lastIndexOf(findableCh)
          }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because lastIndexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, ch: Int, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (numStr.length > 0) {
          val fromIndex = p2 % numStr.length

          numStr.lastIndexOf(ch, fromIndex) shouldEqual
            numStr.value.lastIndexOf(ch, fromIndex)

          val idx = p1 % numStr.length
          val findableCh = numStr.charAt(idx)

          numStr.lastIndexOf(findableCh, fromIndex) shouldEqual
            numStr.value.lastIndexOf(findableCh, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
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
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
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
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
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
    // SKIP-SCALATESTJS,NATIVE-START
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
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer regionMatches methods that are consistent with String") {
      forAll { (numStr: NumericString, i1: Int, str: String, i2: Int, len: Int ) =>
        numStr.regionMatches(true, i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(true, i1, str, i2, len)

        numStr.regionMatches(false, i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(false, i1, str, i2, len)

        numStr.regionMatches(i1, str, i2, len) shouldEqual
          numStr.value.regionMatches(i1, str, i2, len)

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
    // SKIP-SCALATESTJS,NATIVE-START
/*  Please leave this here for other String AnyVals besides NumericString
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
*/
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer a trim method that is consistent with String") {
      forAll { (numStr: NumericString) =>
        numStr.trim shouldEqual
          numStr.value.trim
      }
    }
    it ("should offer a concatNumericString method that takes a NumericString") {
      forAll { (numStr1: NumericString, numStr2: NumericString) =>
        numStr1 concatNumericString numStr2 shouldEqual
          NumericString.from(numStr1.value ++ numStr2.value).get
      }
    }
    it ("should offer a ++ method that takes a String") {
      forAll { (numStr: NumericString, str: String) =>
        numStr ++ str shouldEqual
          numStr.value ++ str
      }
    }
    it ("should offer a * method that takes an Int") {
      forAll { (numStr: NumericString, n: Int) =>
        val aReasonableSizeNumber = n % 1024
        numStr * aReasonableSizeNumber shouldEqual
          NumericString.from(numStr.value * aReasonableSizeNumber).get
      }
    }
    it ("should offer a ++: method that takes a String") {
      forAll { (numStr: NumericString, str: String) =>
        str ++: numStr shouldEqual
          str ++: numStr.value
      }
    }
    it ("should offer a +: method that takes a Char") {
      forAll { (numStr: NumericString, ch: Char) =>
        ch +: numStr shouldEqual
          ch +: numStr.value
      }
    }
    it ("should offer a /: (foldLeft) method that takes an Int as a start value") {
      forAll { (numStr: NumericString, ch: Char) =>
        (ch.toInt /: numStr)(_+_) shouldEqual
          (ch.toInt /: numStr.value)(_+_)
      }
    }
    it ("should offer a :+ method that takes a Char") {
      forAll { (numStr: NumericString, ch: Char) =>
        numStr :+ ch shouldEqual
          numStr.value :+ ch
      }
    }
    it ("should offer a :\\ (foldRight) method that takes an Int as a start value") {
      forAll { (numStr: NumericString, ch: Char) =>
        (numStr :\ ch.toInt)(_+_) shouldEqual
          (numStr.value :\ ch.toInt)(_+_)
      }
    }
    it ("should offer a < method that takes a String") {
      forAll { (numStr: NumericString, arbitraryStr: String) =>
        numStr < arbitraryStr shouldEqual
          numStr.value < arbitraryStr
      }
      forAll { (numStr: NumericString, numericStr: NumericString) =>
        numStr < numericStr.value shouldEqual
          numStr.value < numericStr.value
      }
    }
    it ("should offer a <= method that takes a String") {
      forAll { (numStr: NumericString, arbitraryStr: String) =>
        numStr <= arbitraryStr shouldEqual
          numStr.value <= arbitraryStr
      }
      forAll { (numStr: NumericString, numericStr: NumericString) =>
        numStr <= numericStr.value shouldEqual
          numStr.value <= numericStr.value
      }
    }
    it ("should offer a > method that takes a String") {
      forAll { (numStr: NumericString, arbitrary: String) =>
        numStr > arbitrary shouldEqual
          numStr.value > arbitrary
      }
      forAll { (numStr: NumericString, numeric: NumericString) =>
        numStr > numeric.value shouldEqual
          numStr.value > numeric.value
      }
    }
    it ("should offer a >= method that takes a String") {
      forAll { (numStr: NumericString, arbitrary: String) =>
        numStr >= arbitrary shouldEqual
          numStr.value >= arbitrary
      }
      forAll { (numStr: NumericString, numeric: NumericString) =>
        numStr >= numeric.value shouldEqual
          numStr.value >= numeric.value
      }
    }
    it ("should offer an addString method that takes a StringBuilder") {
      forAll { (numStr: NumericString, str: String) =>
        val sb1 = new StringBuilder(str)
        val sb2 = new StringBuilder(str)

        numStr.addString(sb1) shouldEqual
          numStr.value.addString(sb2)
      }
    }
    it ("should offer an addString method that takes a StringBuilder and a separator String") {
      forAll { (numStr: NumericString, str: String, sep: String) =>
        val sb1 = new StringBuilder(str)
        val sb2 = new StringBuilder(str)

        numStr.addString(sb1, sep) shouldEqual
          numStr.value.addString(sb2, sep)
      }
    }
    it ("should offer an addString method that takes a StringBuilder and start, separator, and end Strings") {
      forAll { (numStr: NumericString, str: String, sep: String, start: String, end: String) =>
        val sb1 = new StringBuilder(str)
        val sb2 = new StringBuilder(str)

        numStr.addString(sb1, start, sep, end) shouldEqual
          numStr.value.addString(sb2, start, sep, end)
      }
    }
    it ("should offer an aggregate method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.aggregate(0)(
          { (sum, ch) => sum + ch.toInt },
          { (p1, p2) => p1 + p2 }
        ) shouldEqual
        aggregate(numStr.value, 0)(
          { (sum, ch) => sum + ch.toInt },
          { (p1, p2) => p1 + p2 }
        )
      }
    }
    it("should offer an apply method that is consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val idx = pint % numStr.length
          numStr(idx) shouldEqual numStr.value(idx)
        }
      }
    }
    it("should offer a canEqual method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.canEqual(NumericString("123")) shouldBe true
        numStr.canEqual(NumericString("")) shouldBe true
        numStr.canEqual("123") shouldBe false
        numStr.canEqual(123) shouldBe false
      }
    }
    it("should offer a capitalize method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.capitalize shouldEqual
          numStr.value.capitalize
      }
    }
/*
    it("should offer a chars method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.chars.toArray shouldEqual
          numStr.value.chars.toArray
      }
    }
    it("should offer a codePoints method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.codePoints.toArray shouldEqual
          numStr.value.codePoints.toArray
      }
    }
*/
    it("should offer a collect method that is consistent with StringOps") {
      val isEven: PartialFunction[Char, Char] = {
        case c if (c - '0') % 2 == 0 => c
      }
      forAll { (numStr: NumericString) =>
        numStr.collect { isEven } shouldEqual
          numStr.value.collect { isEven }
      }
    }
    it("should offer a collectFirst method that is consistent with StringOps") {
      val isEven: PartialFunction[Char, Char] = {
        case c if (c - '0') % 2 == 0 => c
      }
      forAll { (numStr: NumericString) =>
        numStr.collectFirst { isEven } shouldEqual
          numStr.value.collectFirst { isEven }
      }
    }
    it("should offer a combinations method that is consistent with StringOps") {
      forAll { (n1: NumericString, pint: PosInt) =>
        // limit length to prevent out-of-memory errors
        val numStr = NumericString.from(n1.value.take(16)).get

        numStr.combinations(pint).mkString(",") shouldEqual
          numStr.value.combinations(pint).mkString(",")

        whenever (numStr.length > 0) {
          val plausible = pint % numStr.length

          numStr.combinations(plausible).mkString(",") shouldEqual
            numStr.value.combinations(plausible).mkString(",")
        }
      }
    }
    it("should offer a compare method that is consistent with StringOps") {
      forAll { (numStr: NumericString, that: String) =>
        numStr.compare(that) shouldEqual
          numStr.value.compare(that)
      }
    }
    it("should offer a containsSlice method that takes a String") {
      forAll { (numStr: NumericString, that: String, p1: PosInt, p2: PosInt) =>
        numStr.containsSlice(that) shouldEqual
          numStr.value.containsSlice(that)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val substr = numStr.value.substring(beginIndex, endIndex)

          numStr.containsSlice(substr) shouldEqual
            numStr.value.containsSlice(substr)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer a contentEquals method that accepts a StringBuffer") {
      forAll { (numStr: NumericString, str: String) =>
        var sb = new StringBuffer(str)
        numStr.contentEquals(sb) shouldEqual
          numStr.value.contentEquals(sb)

        sb = new StringBuffer(numStr.value)
        numStr.contentEquals(sb) shouldEqual
          numStr.value.contentEquals(sb)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer a copyToArray method that takes start and len args") {
      forAll { (numStr: NumericString, start: PosInt, len: PosInt) =>
        val xs1 = Array.fill[Char](256)(0)
        val xs2 = Array.fill[Char](256)(0)

        numStr.copyToArray(xs1, start, len)
        numStr.value.copyToArray(xs2, start, len)

        xs1 shouldEqual xs2

        whenever (numStr.length > 0) {
          val plausibleStart = start % numStr.length
          val plausibleLen   = len % numStr.length

          numStr.copyToArray(xs1, plausibleStart, plausibleLen)
          numStr.value.copyToArray(xs2, plausibleStart, plausibleLen)

          xs1 shouldEqual xs2
        }
      }
    }
    it("should offer a copyToArray method that takes just an Array arg") {
      forAll { (numStr: NumericString) =>
        val xs1 = Array.fill[Char](256)(0)
        val xs2 = Array.fill[Char](256)(0)

        numStr.copyToArray(xs1)
        numStr.value.copyToArray(xs2, 0)

        xs1 shouldEqual xs2
      }
    }
    it("should offer a copyToArray method that takes a start arg") {
      forAll { (numStr: NumericString, start: PosInt) =>
        val xs1 = Array.fill[Char](256)(0)
        val xs2 = Array.fill[Char](256)(0)

        numStr.copyToArray(xs1, start)
        numStr.value.copyToArray(xs2, start)

        xs1 shouldEqual xs2

        whenever (numStr.length > 0) {
          val plausibleStart = start % numStr.length

          numStr.copyToArray(xs1, plausibleStart)
          numStr.value.copyToArray(xs2, plausibleStart)

          xs1 shouldEqual xs2
        }
      }
    }
    it("should offer a copyToBuffer method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        val dest1 = new ArrayBuffer[Char]
        val dest2 = new ArrayBuffer[Char]

        numStr.copyToBuffer(dest1)
        numStr.value.copyToBuffer(dest2)

        dest1 shouldEqual dest2
      }
    }
    it("should offer a corresponds method consistent with StringOps") {
      def compare(ch1: Char, ch2: Char): Boolean = ch1.toLower == ch2.toLower

      forAll { (numStr: NumericString, str: String) =>
        numStr.corresponds(str)(compare) shouldEqual
          numStr.value.corresponds(str)(compare)

        val matchingSeq = numStr.value.toSeq
        numStr.corresponds(matchingSeq)(compare) shouldEqual
          numStr.value.corresponds(matchingSeq)(compare)
      }
    }
    it("should offer a count method consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString, str: String) =>
        numStr.count(isEven) shouldEqual
          numStr.value.count(isEven)
      }
    }
    it("should offer a diff method consistent with StringOps") {
      forAll { (numStr: NumericString, that: String) =>
        numStr.diff(that) shouldEqual
          numStr.value.diff(that)

        val plausible = Array('0', '1', '1')
        numStr.diff(plausible) shouldEqual
          numStr.value.diff(plausible)
      }
    }
    it("should offer a distinct method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.distinct shouldEqual
          numStr.value.distinct
      }
    }
    it("should offer a drop method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        numStr.drop(pint) shouldEqual
          numStr.value.drop(pint)

        whenever (numStr.length > 0) {
          val plausible = pint % numStr.length

          numStr.drop(plausible) shouldEqual
            numStr.value.drop(plausible)
        }
      }
    }
    it("should offer a dropRight method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        numStr.dropRight(pint) shouldEqual
          numStr.value.dropRight(pint)

        whenever (numStr.length > 0) {
          val plausible = pint % numStr.length

          numStr.dropRight(plausible) shouldEqual
            numStr.value.dropRight(plausible)
        }
      }
    }
    it("should offer a dropWhile method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.dropWhile(lt5) shouldEqual
          numStr.value.dropWhile(lt5)
      }
    }
    it("should offer an endsWith method consistent with StringOps") {
      forAll { (numStr: NumericString, n2: NumericString) =>
        val charSeq = n2.value.toSeq
        numStr.endsWith(charSeq) shouldEqual
          numStr.value.endsWith(charSeq)
      }
    }
    it("should offer an equals method consistent with String") {
      forAll { (numStr: NumericString, n2: NumericString) =>
        numStr.equals(n2) shouldEqual
          numStr.value.equals(n2.value)
      }
    }
    it("should offer an equalsIgnoreCase method consistent with String") {
      forAll { (numStr: NumericString, n2: NumericString) =>
        numStr.equalsIgnoreCase(numStr.value) shouldEqual
          numStr.value.equals(numStr.value)

        numStr.equalsIgnoreCase(n2.value) shouldEqual
          numStr.value.equals(n2.value)
      }
    }
    it("should offer an exists method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.exists(lt5) shouldEqual
          numStr.value.exists(lt5)
      }
    }
    it("should offer a filter method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.filter(lt5) shouldEqual
          numStr.value.filter(lt5)
      }
    }
    it("should offer a filterNot method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.filterNot(lt5) shouldEqual
          numStr.value.filterNot(lt5)
      }
    }
    it("should offer a find method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.find(lt5) shouldEqual
          numStr.value.find(lt5)
      }
    }
    it("should offer a flatMap method consistent with StringOps") {
      def fooIt(c: Char): String = "foo" + c

      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6705
      forAll { (numStr: NumericString) =>
        numStr.flatMap(fooIt).mkString shouldEqual
          numStr.value.flatMap(fooIt _)
      }
      // SKIP-DOTTY-END
    }
    it("should offer a fold method consistent with StringOps") {
      def sumchars(c1: Char, c2:Char): Char = (c1 + c2).toChar

      forAll { (numStr: NumericString) =>
        numStr.fold[Char](0)(sumchars) shouldEqual
          numStr.value.fold[Char](0)(sumchars)
      }
    }
    it("should offer a foldLeft method consistent with StringOps") {
      def sumchars(c1: Char, c2:Char): Char = (c1 + c2).toChar

      forAll { (numStr: NumericString) =>
        numStr.foldLeft[Char](0)(sumchars) shouldEqual
          numStr.value.foldLeft[Char](0)(sumchars)
      }
    }
    it("should offer a foldRight method consistent with StringOps") {
      def sumchars(c1: Char, c2:Char): Char = (c1 + c2).toChar

      forAll { (numStr: NumericString) =>
        numStr.foldRight[Char](0)(sumchars) shouldEqual
          numStr.value.foldRight[Char](0)(sumchars)
      }
    }
    it("should offer a forall method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'

      forAll { (numStr: NumericString) =>
        numStr.forall(lt5) shouldEqual
          numStr.value.forall(lt5)
      }
    }
    it("should offer a foreach method consistent with StringOps") {
      var buf = new StringBuffer
      def buffer(ch: Char) = buf.append(ch)

      forAll { (numStr: NumericString) =>
        buf.setLength(0)
        numStr.foreach(buffer)
        val r1 = buf.toString

        buf.setLength(0)
        numStr.value.foreach(buffer)
        val r2 = buf.toString

        r1 shouldEqual r2
      }
    }
    it("should offer a groupBy method consistent with StringOps") {
      def mod2(ch: Char) = ch % 2

      forAll { (numStr: NumericString) =>
        numStr.groupBy(mod2) shouldEqual
          numStr.value.groupBy(mod2)
      }
    }
    it("should offer a grouped method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        whenever (numStr.length > 0) {
          val size = pint % numStr.length + 1

          numStr.grouped(size).mkString(",") shouldEqual
            numStr.value.grouped(size).mkString(",")
        }
      }
    }
    it("should offer a hasDefiniteSize method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.hasDefiniteSize shouldEqual
          numStr.value.hasDefiniteSize
      }
    }
    it("should offer a head method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.head shouldEqual
            numStr.value.head
        }
      }
    }
    it("should offer a headOption method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.headOption shouldEqual
          numStr.value.headOption
      }
    }
    it("should offer an indexOfSlice method that is consistent with StringOps") {
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        val seq = str.toSeq

        numStr.indexOfSlice(seq) shouldEqual
          numStr.value.indexOfSlice(seq)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableSeq = numStr.substring(beginIndex, endIndex).toSeq

          numStr.indexOfSlice(findableSeq) shouldEqual
            numStr.value.indexOfSlice(findableSeq)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val seq = str.toSeq
          val fromIndex = p3 % numStr.length

          numStr.indexOfSlice(seq, fromIndex) shouldEqual
            numStr.value.indexOfSlice(seq, fromIndex)

          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableSeq = numStr.substring(beginIndex, endIndex).toSeq

          numStr.indexOfSlice(findableSeq, fromIndex) shouldEqual
            numStr.value.indexOfSlice(findableSeq, fromIndex)
        }
      }
    }
    it("should offer an indexWhere method that is consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString) =>
        numStr.indexWhere(isEven) shouldEqual
          numStr.value.indexWhere(isEven)
      }

      forAll { (numStr: NumericString, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p3 % numStr.length

          numStr.indexWhere(isEven, fromIndex) shouldEqual
            numStr.value.indexWhere(isEven, fromIndex)
        }
      }
    }
    it("should offer an indices method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.indices shouldEqual
          numStr.value.indices
      }
    }
    it("should offer an init method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.init shouldEqual
            numStr.value.init
        }
      }
    }
    it("should offer an inits method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.inits.mkString(",") shouldEqual
          numStr.value.inits.mkString(",")
      }
    }
    it("should offer an intersect method consistent with StringOps") {
      forAll { (numStr: NumericString, that: NumericString) =>
        numStr.intersect(that.value) shouldEqual
          numStr.value.intersect(that.value)
      }
    }
    it("should offer an isDefinedAt method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        numStr.isDefinedAt(pint) shouldEqual
          numStr.value.isDefinedAt(pint)
      }
    }
    it("should offer an iterator method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.iterator.mkString(",") shouldEqual
          numStr.value.iterator.mkString(",")
      }
    }
    it("should offer a last method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.last shouldEqual
            numStr.value.last
        }
      }
    }
    it("should offer a lastIndexOfSlice method that is consistent with String") {
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt) =>
        val seq = str.toSeq

        numStr.lastIndexOfSlice(seq) shouldEqual
          numStr.value.lastIndexOfSlice(seq)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableSeq = numStr.substring(beginIndex, endIndex)

          numStr.lastIndexOfSlice(findableSeq) shouldEqual
            numStr.value.lastIndexOfSlice(findableSeq)
        }
      }
      forAll { (numStr: NumericString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        val seq = str.toSeq
        whenever (numStr.length > 0) {
          val fromIndex = p3 % numStr.length

          numStr.lastIndexOfSlice(seq, fromIndex) shouldEqual
            numStr.value.lastIndexOfSlice(seq, fromIndex)

          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableSeq = numStr.substring(beginIndex, endIndex).toSeq

          numStr.lastIndexOfSlice(findableSeq, fromIndex) shouldEqual
            numStr.value.lastIndexOfSlice(findableSeq, fromIndex)
        }
      }
    }
    it("should offer a lastIndexWhere method that is consistent with String") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString) =>
        numStr.lastIndexWhere(isEven) shouldEqual
          numStr.value.lastIndexWhere(isEven)
      }

      forAll { (numStr: NumericString, p3: PosInt) =>
        whenever (numStr.length > 0) {
          val fromIndex = p3 % numStr.length

          numStr.lastIndexWhere(isEven, fromIndex) shouldEqual
            numStr.value.lastIndexWhere(isEven, fromIndex)
        }
      }
    }
    it("should offer a lastOption method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.lastOption shouldEqual
          numStr.value.lastOption
      }
    }
    it("should offer a lengthCompare method consistent with StringOps") {
      forAll { (numStr: NumericString, len: Int) =>
        numStr.lengthCompare(len) shouldEqual
          numStr.value.lengthCompare(len)
      }
    }
    it("should offer a lines method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.lines.mkString(",") shouldEqual
          numStr.value.linesIterator.mkString(",")
      }
    }
    it("should offer a linesWithSeparators method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.linesWithSeparators.mkString(",") shouldEqual
          numStr.value.linesWithSeparators.mkString(",")
      }
    }
    it("should offer a map method consistent with StringOps") {
      def plus1(ch: Char) = (ch + 1).toChar

      forAll { (numStr: NumericString) =>
        numStr.map(plus1) shouldEqual
          numStr.value.map(plus1 _)
      }
    }
    it("should offer a max method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.max shouldEqual
          numStr.value.max
      }
    }
    it("should offer a maxBy method consistent with StringOps") {
      def mod3(ch: Char): Int = ch % 3

      forAll { (numStr: NumericString) =>
        numStr.maxBy(mod3) shouldEqual
          numStr.value.maxBy(mod3)
      }
    }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer a min method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.min shouldEqual
          numStr.value.min
      }
    }
    it("should offer a minBy method consistent with StringOps") {
      def mod3(ch: Char): Int = ch % 3

      forAll { (numStr: NumericString) =>
        numStr.minBy(mod3) shouldEqual
          numStr.value.minBy(mod3)
      }
    }
    it("should offer mkString methods consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.mkString shouldEqual
          numStr.value.mkString

        numStr.mkString(",") shouldEqual
          numStr.value.mkString(",")

        numStr.mkString("<", " ", ">") shouldEqual
          numStr.value.mkString("<", " ", ">")
      }
    }
    // SKIP-DOTTY-END
    it("should offer a nonEmpty method consistent with StringOps") {
      val empty = NumericString("")

      empty.nonEmpty shouldEqual
        empty.value.nonEmpty

      forAll { (numStr: NumericString) =>
        numStr.nonEmpty shouldEqual
          numStr.value.nonEmpty
      }
    }
    it("should offer a padTo method consistent with StringOps") {
      forAll { (numStr: NumericString, len: Int, elem: Char) =>
        val reasonableLen = len % (16 * 1024)
        numStr.padTo(reasonableLen, elem) shouldEqual
          numStr.value.padTo(reasonableLen, elem)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    /*it("should offer a par method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.par shouldEqual
          numStr.value.par
      }
    }*/
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer a partition method consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString) =>
        numStr.partition(isEven) shouldEqual
          numStr.value.partition(isEven)
      }
    }
    it("should offer a patch method consistent with StringOps") {
      forAll { (numStr: NumericString, from: PosZInt, that: String, replaced: PosZInt) =>
        numStr.patch(from, that, replaced % numStr.length) shouldEqual
          numStr.value.patch(from, that, replaced % numStr.length)

        val reasonableFrom = from % numStr.length
        val reasonableReplaced = replaced % numStr.length

        numStr.patch(reasonableFrom, that, reasonableReplaced) shouldEqual
          numStr.value.patch(reasonableFrom, that, reasonableReplaced)
      }
    }
    it("should offer a permutations method that is consistent with StringOps") {
      forAll { (n1: NumericString) =>
        // limit length to prevent out-of-memory errors
        val numStr = NumericString.from(n1.value.take(10)).get

        numStr.permutations.mkString(",") shouldEqual
          numStr.value.permutations.mkString(",")
      }
    }
    it("should offer a prefixLength method that is consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString) =>
        numStr.prefixLength(isEven) shouldEqual
          numStr.value.prefixLength(isEven)
      }
    }
    it("should offer a product method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.product shouldEqual
          numStr.value.product
      }
    }
    it("should offer an r method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.r().toString shouldEqual
          numStr.value.r().toString

        numStr.r.toString shouldEqual
          numStr.value.r.toString
      }
    }
    it("should offer a reduce method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduce(max) shouldEqual
            numStr.value.reduce(max)
        }
      }
    }
    it("should offer a reduceLeft method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduceLeft(max) shouldEqual
            numStr.value.reduceLeft(max)
        }
      }
    }
    it("should offer a reduceLeftOption method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      val emptyNumStr = NumericString("")
      emptyNumStr.reduceLeftOption(max) shouldEqual
        emptyNumStr.value.reduceLeftOption(max)

      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduceLeftOption(max) shouldEqual
            numStr.value.reduceLeftOption(max)
        }
      }
    }
    it("should offer a reduceOption method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      val emptyNumStr = NumericString("")
      emptyNumStr.reduceOption(max) shouldEqual
        emptyNumStr.value.reduceOption(max)

      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduceOption(max) shouldEqual
            numStr.value.reduceOption(max)
        }
      }
    }
    it("should offer a reduceRight method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduceRight(max) shouldEqual
            numStr.value.reduceRight(max)
        }
      }
    }
    it("should offer a reduceRightOption method that is consistent with StringOps") {
      def max(c1: Char, c2: Char) = math.max(c1, c2).toChar

      val emptyNumStr = NumericString("")
      emptyNumStr.reduceRightOption(max) shouldEqual
        emptyNumStr.value.reduceRightOption(max)

      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6705
      forAll { (numStr: NumericString) =>
        whenever (numStr.length > 0) {
          numStr.reduceRightOption(max) shouldEqual
            numStr.value.reduceRightOption(max)
        }
      }
      // SKIP-DOTTY-END
    }
    it("should offer a replaceAllLiterally method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.replaceAllLiterally("0+", "1") shouldEqual
          numStr.value.replaceAllLiterally("0+", "1")
      }
    }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer a reverse method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.reverse shouldEqual
          numStr.value.reverse
      }
    }
    it("should offer a reverseMap method consistent with StringOps") {
      def plus1(ch: Char) = (ch + 1).toChar

      forAll { (numStr: NumericString) =>
        numStr.reverseMap(plus1).mkString shouldEqual
          numStr.value.reverseMap(plus1).mkString
      }
    }
    // SKIP-DOTTY-END
    it("should offer a scan method consistent with StringOps") {
      def sum(c1: Char, c2: Char) = (c1 + c2).toChar

      forAll { (numStr: NumericString) =>
        numStr.scan('0')(sum) shouldEqual
          numStr.value.scan('0')(sum)
      }
    }
    it("should offer a scanLeft method consistent with StringOps") {
      def sum(s: String, c2: Char) = s + c2

      forAll { (numStr: NumericString) =>
        numStr.scanLeft("0")(sum) shouldEqual
          numStr.value.scanLeft("0")(sum)
      }
    }
    it("should offer a scanRight method consistent with StringOps") {
      def sum(c1: Char, s: String) = c1.toString + s

      forAll { (numStr: NumericString) =>
        numStr.scanRight("0")(sum) shouldEqual
          numStr.value.scanRight("0")(sum)
      }
    }
    it("should offer a sameElements method consistent with StringOps") {
      forAll { (numStr: NumericString, str: String) =>
        numStr.sameElements(str) shouldEqual
          numStr.value.sameElements(str)

        numStr.sameElements(numStr.value.toSeq) shouldEqual
          numStr.value.sameElements(numStr.value.toSeq)
      }
    }
    it("should offer a segmentLength method that is consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString, pint: PosInt) =>
        numStr.segmentLength(isEven, pint) shouldEqual
          numStr.value.segmentLength(isEven, pint)

        whenever (numStr.length > 0) {
          val plausible = pint % numStr.length

          numStr.segmentLength(isEven, plausible) shouldEqual
            numStr.value.segmentLength(isEven, plausible)
        }
      }
    }
    it("should offer a seq method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.seq shouldEqual
          numStr.value.seq
      }
    }
    it("should offer a size method that is consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.size shouldEqual
          numStr.value.size
      }
    }
    it("should offer a slice method consistent with StringOps") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        numStr.slice(p1, p2) shouldEqual
          numStr.value.slice(p1, p2)

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length
          val idx2 = p2 % numStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          numStr.slice(beginIndex, endIndex) shouldEqual
            numStr.value.slice(beginIndex, endIndex)
        }
      }
    }
    it("should offer sliding methods consistent with StringOps") {
      forAll { (numStr: NumericString, p1: PosInt, p2: PosInt) =>
        numStr.sliding(p1, p2).toList shouldEqual
          numStr.value.sliding(p1, p2).toList

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length + 1
          val idx2 = p2 % numStr.length + 1

          numStr.sliding(idx1, idx2).toList shouldEqual
            numStr.value.sliding(idx1, idx2).toList
        }
      }
      forAll { (numStr: NumericString, p1: PosInt) =>
        numStr.sliding(p1).toList shouldEqual
          numStr.value.sliding(p1).toList

        whenever (numStr.length > 0) {
          val idx1 = p1 % numStr.length + 1

          numStr.sliding(idx1).toList shouldEqual
            numStr.value.sliding(idx1).toList
        }
      }
    }
    it("should offer a sortBy method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        def identity = (c: Char) => c

        numStr.sortBy(identity) shouldEqual
          numStr.value.sortBy(identity)
      }
    }
    it("should offer a sortWith method consistent with StringOps") {
      def lt = (c1: Char, c2: Char) => c1 < c2

      forAll { (numStr: NumericString) =>
        numStr.sortWith(lt) shouldEqual
          numStr.value.sortWith(lt)
      }
    }
    it("should offer a sorted method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.sorted shouldEqual
          numStr.value.sorted
      }
    }
    it("should offer a span method consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString, str: String) =>
        numStr.span(isEven) shouldEqual
          numStr.value.span(isEven)
      }
    }
    it("should offer split methods consistent with StringOps") {
      val separators = Array('0', '2', '4')
      forAll { (numStr: NumericString) =>
        numStr.split(separators) shouldEqual
          numStr.value.split(separators)
      }

      val separator = '5'
      forAll { (numStr: NumericString) =>
        numStr.split(separator) shouldEqual
          numStr.value.split(separator)
      }
    }
    it("should offer a splitAt method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt) =>
        numStr.splitAt(pint) shouldEqual
          numStr.value.splitAt(pint)

        whenever (numStr.length > 0) {
          val n = pint % numStr.length

          numStr.splitAt(n) shouldEqual
            numStr.value.splitAt(n)
        }
      }
    }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer a stringPrefix method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.stringPrefix shouldEqual
          org.scalactic.ColCompatHelper.className(numStr.value)
      }
    }
    it("should offer a stripLineEnd method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.stripLineEnd shouldEqual
          numStr.value.stripLineEnd
      }
    }
    it("should offer stripMargin methods consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.stripMargin shouldEqual
          numStr.value.stripMargin

        val marginChar = '1'
        numStr.stripMargin(marginChar) shouldEqual
          numStr.value.stripMargin(marginChar)
      }
    }
    // SKIP-DOTTY-END
    it("should offer a stripPrefix method consistent with StringOps") {
      forAll { (numStr: NumericString, str: String) =>
        numStr.stripPrefix(str) shouldEqual
          numStr.value.stripPrefix(str)

        whenever (numStr.length > 1) {
          val prefix = numStr.value.substring(0, 2)

          numStr.stripPrefix(prefix) shouldEqual
            numStr.value.stripPrefix(prefix)
        }
      }
    }
    it("should offer a stripSuffix method consistent with StringOps") {
      forAll { (numStr: NumericString, str: String) =>
        numStr.stripSuffix(str) shouldEqual
          numStr.value.stripSuffix(str)

        whenever (numStr.length > 1) {
          val suffix = numStr.value.substring(numStr.length - 2)

          numStr.stripSuffix(suffix) shouldEqual
            numStr.value.stripSuffix(suffix)
        }
      }
    }
    it("should offer a sum method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.sum shouldEqual
          numStr.value.sum
      }
    }
    it("should offer a tail method consistent with StringOps") {
      forAll { (numStr: NumericString) =>

        whenever (numStr.length > 0) {
          numStr.tail shouldEqual
            numStr.value.tail
        }
      }
    }
    it("should offer a tails method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.tails.toList shouldEqual
          numStr.value.tails.toList
      }
    }
    it("should offer a take method consistent with StringOps") {
      forAll { (numStr: NumericString, n: Int) =>
        numStr.take(n) shouldEqual
          numStr.value.take(n)
      }
    }
    it("should offer a takeRight method consistent with StringOps") {
      forAll { (numStr: NumericString, n: Int) =>
        numStr.takeRight(n) shouldEqual
          numStr.value.takeRight(n)
      }
    }
    it("should offer a takeWhile method consistent with StringOps") {
      def isEven(ch: Char): Boolean = (ch - '0') % 2 == 0

      forAll { (numStr: NumericString) =>
        numStr.takeWhile(isEven) shouldEqual
          numStr.value.takeWhile(isEven)
      }
    }
    it("should offer a toArray method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toArray shouldEqual
          numStr.value.toArray
      }
    }
    it("should offer a toBuffer method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toBuffer[Char] shouldEqual
          numStr.value.toBuffer[Char]
      }
    }
    it("should offer a toByte method consistent with StringOps") {
      forAll { (b: Byte) =>
        whenever (b >= 0) {
          val numStr = NumericString.from(b.toString).get

          numStr.toByte shouldEqual
            numStr.value.toByte
        }
      }
    }
    it("should offer a toDouble method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toDouble shouldEqual
          numStr.value.toDouble
      }
    }
    it("should offer a toFloat method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toFloat shouldEqual
          numStr.value.toFloat
      }
    }
    it("should offer a toIndexedSeq method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toIndexedSeq shouldEqual
          numStr.value.toIndexedSeq
      }
    }
    it("should offer a toInt method consistent with StringOps") {
      forAll { (n: Int) =>
        whenever (n >= 0) {
          val numStr = NumericString.from(n.toString).get

          numStr.toInt shouldEqual
            numStr.value.toInt
        }
      }
    }
    it("should offer a toIterable method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toIterable shouldEqual
          numStr.value.toIterable
      }
    }
    it("should offer a toIterator method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toIterator.toList shouldEqual
          numStr.value.toIterator.toList
      }
    }
    it("should offer a toList method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toList shouldEqual
          numStr.value.toList
      }
    }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer a toLong method consistent with StringOps") {
      forAll { (n: Long) =>
        whenever (n >= 0) {
          val numStr = NumericString.from(n.toString).get

          numStr.toLong shouldEqual
            numStr.value.toLong
        }
      }
    }
    // SKIP-DOTTY-END
    it("should offer a toSeq method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toSeq shouldEqual
          numStr.value.toSeq
      }
    }
    it("should offer a toSet method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toSet shouldEqual
          numStr.value.toSet
      }
    }
    it("should offer a toShort method consistent with StringOps") {
      forAll { (s: Short) =>
        whenever (s >= 0) {
          val numStr = NumericString.from(s.toString).get

          numStr.toShort shouldEqual
            numStr.value.toShort
        }
      }
    }
    it("should offer a toStream method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toStream shouldEqual
          numStr.value.toStream
      }
    }
    it("should offer a toVector method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.toVector shouldEqual
          numStr.value.toVector
      }
    }
    it("should offer a union method consistent with StringOps") {
      forAll { (numStr: NumericString, that: NumericString) =>
        numStr.union(that.value) shouldEqual
          numStr.value.union(that.value)
      }
    }
    // SKIP-DOTTY-START
    // type checking error for `numStr.updated(index, c).value shouldEqual`
    // value is not a member of scala.collection.immutable.IndexedSeq[AnyVal]
    it("should offer a updated method consistent with StringOps") {
      forAll { (numStr: NumericString, pint: PosInt, c: NumericChar) =>
        whenever (numStr.length > 0) {
          val index = pint % numStr.length

          numStr.updated(index, c).value shouldEqual
            numStr.value.updated(index, c.value)
        }
      }
    }
    // SKIP-DOTTY-END

    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer view methods consistent with StringOps") {
      forAll { (numStr: NumericString) =>

        numStr.view shouldEqual
          numStr.value.view

      }
    }
    // SKIP-DOTTY-END
    it("should offer a withFilter method consistent with StringOps") {
      def lt5(ch: Char) = ch < '5'
      def identity(ch: Char) = ch

      forAll { (numStr: NumericString) =>
        numStr.withFilter(lt5).map(identity _) shouldEqual
          numStr.value.withFilter(lt5).map(identity _)
      }
    }
    it("should offer a zip method consistent with StringOps") {
      forAll { (numStr: NumericString, str: String) =>
        numStr.zip(str) shouldEqual
          numStr.value.zip(str)

        numStr.zip(str.toList) shouldEqual
          numStr.value.zip(str.toList)
      }
    }
    it("should offer a zipAll method consistent with StringOps") {
      forAll { (numStr: NumericString, str: String, thisElem: Char, thatElem: Char) =>
        numStr.zipAll(str, thisElem, thatElem) shouldEqual
          numStr.value.zipAll(str, thisElem, thatElem)

        numStr.zipAll(str.toList, thisElem, thatElem) shouldEqual
          numStr.value.zipAll(str.toList, thisElem, thatElem)
      }
    }
    it("should offer a zipWithIndex method consistent with StringOps") {
      forAll { (numStr: NumericString) =>
        numStr.zipWithIndex shouldEqual
          numStr.value.zipWithIndex
      }
    }
    describe("should offer a tryingValid factory method that") {
      it ("returns a NumericString wrapped in a Success if the passed String "+
          "contains only numeric characters") {
        NumericString.tryingValid("123").success.value.value shouldBe "123"
        NumericString.tryingValid("0").success.value.value shouldBe "0"
        NumericString.tryingValid("007").success.value.value shouldBe "007"
        NumericString.tryingValid("").success.value.value shouldBe ""
      }
      it (" returns an AssertionError wrapped in a Failure if the passed "+
          "String does not contain only numeric characters") {
        NumericString.tryingValid("-123").failure.exception shouldBe
          an [AssertionError]
        NumericString.tryingValid("+123").failure.exception shouldBe
          an [AssertionError]
        NumericString.tryingValid("abc").failure.exception shouldBe
          an [AssertionError]
        NumericString.tryingValid("1e14").failure.exception shouldBe
          an [AssertionError]
      }
    }
    describe("should offer a passOrElse factory method that") {
      it ("returns a Pass if the given String contains only numeric characters") {
        NumericString.passOrElse("50")(i => i) shouldBe Pass
        NumericString.passOrElse("0")(i => i) shouldBe Pass
        NumericString.passOrElse("007")(i => i) shouldBe Pass
        NumericString.passOrElse("")(i => i) shouldBe Pass
      }
      it (" returns an error value produced by passing the given String to "+
          "the given function if the passed String is NOT numeric, wrapped "+
          "in a Fail") {
        NumericString.passOrElse("-1")(i => s"$i did not taste good") shouldBe
          Fail("-1 did not taste good")
        NumericString.passOrElse("+1")(i => s"$i did not taste good") shouldBe
          Fail("+1 did not taste good")
        NumericString.passOrElse("broccoli")(i => s"$i did not taste good") shouldBe
          Fail("broccoli did not taste good")
        NumericString.passOrElse("1E-1")(i => s"$i did not taste good") shouldBe
          Fail("1E-1 did not taste good")
      }
    }
    describe("should offer a goodOrElse factory method that") {
      it ("returns a NumericString wrapped in a Good if the given String "+
          "contains only numeric characters") {
        NumericString.goodOrElse("50")(i => i) shouldBe
          Good(NumericString("50"))
        NumericString.goodOrElse("100")(i => i) shouldBe
          Good(NumericString("100"))
        NumericString.goodOrElse("")(i => i) shouldBe
          Good(NumericString(""))
        NumericString.goodOrElse("0")(i => i) shouldBe
          Good(NumericString("0"))
        NumericString.goodOrElse("00")(i => i) shouldBe
          Good(NumericString("00"))
      }
      it ("returns an error value produced by passing the given String to "+
          "the given function if the passed String does not contains only "+
          "numeric characters, wrapped in a Bad") {
        NumericString.goodOrElse("-1")(i => s"$i did not taste good") shouldBe
          Bad("-1 did not taste good")
        NumericString.goodOrElse("+1")(i => s"$i did not taste good") shouldBe
          Bad("+1 did not taste good")
        NumericString.goodOrElse("salamander")(i => s"$i did not taste good") shouldBe
          Bad("salamander did not taste good")
        NumericString.goodOrElse("1e0")(i => s"$i did not taste good") shouldBe
          Bad("1e0 did not taste good")
      }
    }
    describe("should offer a rightOrElse factory method that") {
      it("returns a NumericString wrapped in a Right if the given String does "+
         "not contain only numeric characters") {
        NumericString.rightOrElse("0")(i => i) shouldBe
          Right(NumericString("0"))
        NumericString.rightOrElse("")(i => i) shouldBe
          Right(NumericString(""))
        NumericString.rightOrElse("00")(i => i) shouldBe
          Right(NumericString("00"))
        NumericString.rightOrElse("456")(i => i) shouldBe
          Right(NumericString("456"))
      }
      it ("returns an error value produced by passing the given String to "+
          "the given function if the passed String does not contain only "+
          "numeric characters, wrapped in a Left") {
        NumericString.rightOrElse("-1")(i => s"$i did not taste good") shouldBe
          Left("-1 did not taste good")
        NumericString.rightOrElse("-0")(i => s"$i did not taste good") shouldBe
          Left("-0 did not taste good")
        NumericString.rightOrElse("+0")(i => s"$i did not taste good") shouldBe
          Left("+0 did not taste good")
        NumericString.rightOrElse("that last clam")(i => s"$i did not taste good") shouldBe
          Left("that last clam did not taste good")
        NumericString.rightOrElse("0e0")(i => s"$i did not taste good") shouldBe
          Left("0e0 did not taste good")
      }
    }
  }
}

