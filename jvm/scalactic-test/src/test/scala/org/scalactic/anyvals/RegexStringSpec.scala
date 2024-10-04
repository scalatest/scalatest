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

import org.scalatest._
import org.scalatest.prop._
import OptionValues._
import java.nio.charset.Charset
// SKIP-SCALATESTJS,NATIVE-START
import java.util.Locale
// SKIP-SCALATESTJS,NATIVE-END


class RegexStringSpec extends funspec.AnyFunSpec with matchers.should.Matchers with GeneratorDrivenPropertyChecks {

  import prop._

  implicit val RegexStringGen: Generator[RegexString] =
    specificValues(
      RegexString(""),
      RegexString("."),
      RegexString(".*"),
      RegexString("^Now is the time for all good men$"),
      RegexString("(a|b)"),
      RegexString("""^\\(&amp;|\W|\p{Alpha}+\*?|_)"""),
      RegexString("[abc]"))

  describe("A RegexString") {

    describe("should offer a from factory method that") {
      it("returns Some[RegexString] if the passed String contains a valid regex") {
        RegexString.from("").value.value shouldBe ""
        RegexString.from(".").value.value shouldBe "."
        RegexString.from(".*").value.value shouldBe ".*"
        RegexString.from("[abc]").value.value shouldBe "[abc]"
        RegexString.from("(a|b)").value.value shouldBe "(a|b)"
        RegexString.from("(a|b)+").value.value shouldBe "(a|b)+"
        RegexString.from("""\p{Blank}""").value.value shouldBe """\p{Blank}"""
      }

      it("returns None if the passed String is not a valid regex") {
        RegexString.from("+") shouldBe None
        RegexString.from("(a|b") shouldBe None
        RegexString.from("(a|+)") shouldBe None
        RegexString.from("(") shouldBe None
      }
    }
    describe("should offer an ensuringValid factory method that") {
      it("returns a RegexString if the passed String contains a valid regex") {
        RegexString.ensuringValid("").value shouldBe ""
        RegexString.ensuringValid(".").value shouldBe "."
        RegexString.ensuringValid(".*").value shouldBe ".*"
        RegexString.ensuringValid("[abc]").value shouldBe "[abc]"
        RegexString.ensuringValid("(a|b)").value shouldBe "(a|b)"
        RegexString.ensuringValid("(a|b)+").value shouldBe "(a|b)+"
      }

      it("throws AssertionError if the passed String is not a valid regex") {
        an [AssertionError] should be thrownBy RegexString.ensuringValid("(")
        an [AssertionError] should be thrownBy RegexString.ensuringValid("+")
        an [AssertionError] should be thrownBy RegexString.ensuringValid("99)")
      }
    }
    describe("should offer an isValid predicate method that") {
      it("returns true if the passed String is a valid regex") {
        RegexString.isValid("") shouldBe true
        RegexString.isValid(".") shouldBe true
        RegexString.isValid("a+") shouldBe true
        RegexString.isValid("[abc]") shouldBe true
        RegexString.isValid("(a|b)") shouldBe true
        RegexString.isValid("false") shouldBe true
      }
      it("returns false if the passed String is not a valid regex") {
        RegexString.isValid("a|b)") shouldBe false
        RegexString.isValid("*") shouldBe false
        RegexString.isValid("+") shouldBe false
        RegexString.isValid("?") shouldBe false
        RegexString.isValid("*true") shouldBe false
      }
    }
    describe("should offer a fromOrElse factory method that") {
      it("returns a RegexString if the passed regex is valid") {
        RegexString.fromOrElse("50", RegexString("42")).value shouldBe "50"
        RegexString.fromOrElse("100", RegexString("42")).value shouldBe "100"
      }
      it("returns a given default if the passed regex is not valid") {
        RegexString.fromOrElse(")", RegexString("42")).value shouldBe "42"
        RegexString.fromOrElse("+", RegexString("42")).value shouldBe "42"
        RegexString.fromOrElse("(a|b", RegexString("42")).value shouldBe "42"
      }
    }
    it("should offer an ensuringValid method that takes a String => String, throwing AssertionError if the result is invalid") {
      RegexString("33").ensuringValid(s => (s.toInt + 1).toString) shouldEqual RegexString("34")
      an [AssertionError] should be thrownBy { RegexString("33").ensuringValid(_ + "(") }
    }
    it("should offer a length method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.length shouldEqual regStr.value.length
      }
    }
    it("should offer a charAt method that is consistent with String") {
      forAll { (regStr: RegexString, pint: PosInt) =>
        whenever (regStr.length > 0) {
          val idx = pint % regStr.length
          regStr.charAt(idx) shouldEqual regStr.value.charAt(idx)
        }
      }
    }
    it("should offer a codePointAt method that is consistent with String") {
      forAll { (regStr: RegexString, pint: PosInt) =>
        whenever (regStr.length > 0) {
          val idx = pint % regStr.length
          regStr.codePointAt(idx) shouldEqual regStr.value.codePointAt(idx)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer a codePointBefore method that is consistent with String") {
      forAll { (regStr: RegexString, pint: PosInt) =>
        whenever (regStr.length > 0) {
          val idx = (pint % regStr.length) + 1

          regStr.codePointBefore(idx) shouldEqual
            regStr.value.codePointBefore(idx)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer a codePointCount method that is consistent with String") {
      forAll { (regStr: RegexString, p1: PosInt, p2: PosInt) =>
        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2)

          regStr.codePointCount(beginIndex, endIndex) shouldEqual
            regStr.value.codePointCount(beginIndex, endIndex)
        }
      }
    }
    it("should offer a compareTo method that is consistent with String") {
      forAll { (regStr: RegexString, anotherString: String) =>
        regStr.compareTo(anotherString) shouldEqual
          regStr.value.compareTo(anotherString)

        val sameStr = new String(regStr.value)

        regStr.compareTo(sameStr) shouldEqual
          regStr.value.compareTo(sameStr)
      }
    }
    it("should offer a compareToIgnoreCase method that is consistent with String") {
      forAll { (regStr: RegexString, anotherString: String) =>
        regStr.compareToIgnoreCase(anotherString) shouldEqual
          regStr.value.compareToIgnoreCase(anotherString)

        val sameStr = new String(regStr.value)

        regStr.compareToIgnoreCase(sameStr) shouldEqual
          regStr.value.compareToIgnoreCase(sameStr)
      }
    }
    it("should offer a concat method that is consistent with String") {
      forAll { (regStr: RegexString, str: String) =>
        regStr.concat(str) shouldEqual
          regStr.value.concat(str)
      }
    }
    it("should offer a contains method that is consistent with String") {
      forAll { (regStr: RegexString, s: String, p1: PosInt, p2: PosInt) =>
        regStr.contains(s) shouldEqual
          regStr.value.contains(s)

        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val substr = regStr.value.substring(beginIndex, endIndex)

          regStr.contains(substr) shouldEqual
            regStr.value.contains(substr)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer a contentEquals method that is consistent with String") {
      forAll { (regStr: RegexString, str: String) =>
        val cs: CharSequence = str
        val matchingCs: CharSequence = regStr.value

        regStr.contentEquals(cs) shouldEqual
          regStr.value.contentEquals(cs)

        regStr.contentEquals(matchingCs) shouldEqual
          regStr.value.contentEquals(matchingCs)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer an endsWith method that is consistent with String") {
      forAll { (regStr: RegexString, str: String, p1: PosInt) =>
        regStr.endsWith(str) shouldEqual
          regStr.value.endsWith(str)

        whenever (regStr.length > 0) {
          val idx = p1 % regStr.length
          val endStr = regStr.substring(idx)

          regStr.endsWith(str) shouldEqual
            regStr.value.endsWith(str)
        }
      }
    }
    it("should offer an equals method that is consistent with String") {
      forAll { (regStr: RegexString, regStr2: RegexString) =>
        regStr.equals(regStr) shouldEqual
          regStr.value.equals(regStr.value)

        regStr.equals(regStr2) shouldEqual
          regStr.value.equals(regStr2.value)
      }
    }
    it("should offer a getBytes method that is consistent with String") {
      // SKIP-DOTTY-START
      // https://github.com/lampepfl/dotty/issues/6705
      forAll { (regStr: RegexString) =>
        regStr.getBytes shouldEqual
          regStr.value.getBytes
      }
      // SKIP-DOTTY-END
      forAll { (regStr: RegexString) =>
        regStr.getBytes(Charset.defaultCharset) shouldEqual
          regStr.value.getBytes(Charset.defaultCharset)
      }
      forAll { (regStr: RegexString) =>
        regStr.getBytes("UTF-16") shouldEqual
          regStr.value.getBytes("UTF-16")
      }
    }
    it("should offer a getChars method that is consistent with String") {
      forAll { (regStr: RegexString, p1: PosInt, p2: PosInt, p3: PosInt) =>
        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val srcBegin = math.min(idx1, idx2)
          val srcEnd   = math.max(idx1, idx2) + 1

          // restrict size of dest array to avoid exceeding memory capacity
          val dstBegin = math.min(p3, 10 * regStr.length)
          val dstSize = dstBegin + regStr.length

          val dst1 = Array.fill[Char](dstSize)('-')
          val dst2 = Array.fill[Char](dstSize)('-')

          regStr.getChars(srcBegin, srcEnd, dst1, dstBegin)
          regStr.value.getChars(srcBegin, srcEnd, dst2, dstBegin)

          dst1 shouldEqual dst2
        }
      }
    }
    it("should offer an indexOf method that is consistent with String") {
      forAll { (regStr: RegexString, i1: Int, p1: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        regStr.indexOf(i1) shouldEqual
          regStr.value.indexOf(i1)

        whenever (regStr.length > 0) {
          val idx = p1 % regStr.length
          val findableCh = regStr.charAt(idx)

          regStr.indexOf(findableCh) shouldEqual
            regStr.value.indexOf(findableCh)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, ch: Int, p1: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (regStr.length > 0) {
          val fromIndex = p1 % regStr.length

          regStr.indexOf(ch, fromIndex) shouldEqual
            regStr.value.indexOf(ch, fromIndex)

          val idx = p1 % regStr.length
          val findableCh = regStr.charAt(idx)

          regStr.indexOf(findableCh, fromIndex) shouldEqual
            regStr.value.indexOf(findableCh, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, str: String, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        regStr.indexOf(str) shouldEqual
          regStr.value.indexOf(str)

        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = regStr.substring(beginIndex, endIndex)

          regStr.indexOf(findableStr) shouldEqual
            regStr.value.indexOf(findableStr)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (regStr.length > 0) {
          val fromIndex = p3 % regStr.length

          regStr.indexOf(str, fromIndex) shouldEqual
            regStr.value.indexOf(str, fromIndex)

          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = regStr.substring(beginIndex, endIndex)

          regStr.indexOf(findableStr, fromIndex) shouldEqual
            regStr.value.indexOf(findableStr, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
    }
    it("should offer a intern method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.intern shouldEqual
          regStr.value.intern
      }
    }
    it("should offer an isEmpty method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.isEmpty shouldEqual
          regStr.value.isEmpty
      }
    }
    it("should offer a lastIndexOf method that is consistent with String") {
      forAll { (regStr: RegexString, ch: Int, p1: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        regStr.lastIndexOf(ch) shouldEqual
          regStr.value.lastIndexOf(ch)

        whenever (regStr.length > 0) {
          val idx = p1 % regStr.length
          val findableCh = regStr.charAt(idx)

          regStr.lastIndexOf(findableCh) shouldEqual
            regStr.value.lastIndexOf(findableCh)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, ch: Int, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (regStr.length > 0) {
          val fromIndex = p2 % regStr.length

          regStr.lastIndexOf(ch, fromIndex) shouldEqual
            regStr.value.lastIndexOf(ch, fromIndex)

          val idx = p1 % regStr.length
          val findableCh = regStr.charAt(idx)

          regStr.lastIndexOf(findableCh, fromIndex) shouldEqual
            regStr.value.lastIndexOf(findableCh, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, str: String, p1: PosInt, p2: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        regStr.lastIndexOf(str) shouldEqual
          regStr.value.lastIndexOf(str)

        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = regStr.substring(beginIndex, endIndex)

          regStr.lastIndexOf(findableStr) shouldEqual
            regStr.value.lastIndexOf(findableStr)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
      forAll { (regStr: RegexString, str: String, p1: PosInt, p2: PosInt, p3: PosInt) =>
        //SCALATESTJS,NATIVE-ONLY try {
        whenever (regStr.length > 0) {
          val fromIndex = p3 % regStr.length

          regStr.lastIndexOf(str, fromIndex) shouldEqual
            regStr.value.lastIndexOf(str, fromIndex)

          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          val findableStr = regStr.substring(beginIndex, endIndex)

          regStr.lastIndexOf(findableStr, fromIndex) shouldEqual
            regStr.value.lastIndexOf(findableStr, fromIndex)
        }
        //SCALATESTJS,NATIVE-ONLY }
        //SCALATESTJS,NATIVE-ONLY catch { // This smells like a Scala.js bug, because indexOf throws IllegalArgumentException
        //SCALATESTJS,NATIVE-ONLY   case iae: IllegalArgumentException => succeed
        //SCALATESTJS,NATIVE-ONLY }
      }
    }
    it("should offer a matches method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        val r1 = """[0-9]+"""
        val r2 = """[a-z]+"""

        regStr.matches(r1) shouldEqual
          regStr.value.matches(r1)
        regStr.matches(r2) shouldEqual
          regStr.value.matches(r2)
      }
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should offer a offsetByCodePoints method that is consistent with String") {
      forAll { (regStr: RegexString, p1: PosInt, p2: PosInt) =>
        whenever (regStr.length > 0) {
          val index = p1 % regStr.length
          val codePointOffset = (p2 % regStr.length) - index

          regStr.offsetByCodePoints(index, codePointOffset) shouldEqual
            regStr.value.offsetByCodePoints(index, codePointOffset)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    it("should offer regionMatches methods that are consistent with String") {
      forAll { (regStr: RegexString, i1: Int, str: String, i2: Int, len: Int ) =>
        regStr.regionMatches(true, i1, str, i2, len) shouldEqual
          regStr.value.regionMatches(true, i1, str, i2, len)

        regStr.regionMatches(false, i1, str, i2, len) shouldEqual
          regStr.value.regionMatches(false, i1, str, i2, len)

        regStr.regionMatches(i1, str, i2, len) shouldEqual
          regStr.value.regionMatches(true, i1, str, i2, len)

        whenever (regStr.length > 0) { // test a true condition
          val p1 = math.abs(i1/2) // /2 to avoid abs(Int.MinValue) problem
          val p2 = math.abs(i2/2)

          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val offset = math.min(idx1, idx2)
          val len = math.max(idx1, idx2) - offset + 1
          val sameStr = new String(regStr.value)

          regStr.regionMatches(false, offset, sameStr, offset, len) shouldEqual
            regStr.value.regionMatches(false, offset, sameStr, offset, len)

          regStr.regionMatches(offset, sameStr, offset, len) shouldEqual
            regStr.value.regionMatches(offset, sameStr, offset, len)
        }
      }
    }
    it("should offer replace methods that are consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.replace('*', '+') shouldEqual
          regStr.value.replace('*', '+')
        regStr.replace('9', '1') shouldEqual
          regStr.value.replace('9', '1')
        regStr.replace('a', '1') shouldEqual
          regStr.value.replace('a', '1')
        regStr.replace(".*", "a+") shouldEqual
          regStr.value.replace(".*", "a+")
        regStr.replace("a", "123") shouldEqual
          regStr.value.replace("a", "123")
        regStr.replace("the", "a") shouldEqual
          regStr.value.replace("the", "a")
        regStr.replace("", "4") shouldEqual
          regStr.value.replace("", "4")
      }
    }
    it("should offer a replaceAll method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.replaceAll("a+", "1")shouldEqual
          regStr.value.replaceAll("a+", "1")
      }
    }
    it("should offer a replaceFirst method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.replaceFirst("a+", "1")shouldEqual
          regStr.value.replaceFirst("a+", "1")
      }
    }
    it("should offer split methods that are consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.split("a+") shouldEqual
          regStr.value.split("a+")
        regStr.split("a+", -1) shouldEqual
          regStr.value.split("a+", -1)
        regStr.split("a+", 0) shouldEqual
          regStr.value.split("a+", 0)
        regStr.split("a+", 1) shouldEqual
          regStr.value.split("a+", 1)
        regStr.split("a+", 2) shouldEqual
          regStr.value.split("a+", 2)
      }
    }
    it("should offer startsWith methods that are consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.startsWith("N") shouldEqual
          regStr.value.startsWith("N")
        regStr.startsWith("Now") shouldEqual
          regStr.value.startsWith("Now")

        regStr.startsWith("0", 0) shouldEqual
          regStr.value.startsWith("0", 0)
        regStr.startsWith("Now", 20) shouldEqual
          regStr.value.startsWith("Now", 20)
        regStr.startsWith("for", -20) shouldEqual
          regStr.value.startsWith("for", -20)
      }
    }
    it("should offer a subSequence method that is consistent with String") {
      forAll { (regStr: RegexString, p1: PosInt, p2: PosInt) =>
        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          regStr.subSequence(beginIndex, endIndex) shouldEqual
            regStr.value.subSequence(beginIndex, endIndex)
        }
      }
    }
    it("should offer substring methods that are consistent with String") {
      forAll { (regStr: RegexString, p1: PosInt, p2: PosInt) =>
        whenever (regStr.length > 0) {
          val idx1 = p1 % regStr.length
          val idx2 = p2 % regStr.length

          val beginIndex = math.min(idx1, idx2)
          val endIndex = math.max(idx1, idx2) + 1

          regStr.substring(beginIndex) shouldEqual
            regStr.value.substring(beginIndex)

          regStr.substring(beginIndex, endIndex) shouldEqual
            regStr.value.substring(beginIndex, endIndex)
        }
      }
    }
    // SKIP-DOTTY-START
    // https://github.com/lampepfl/dotty/issues/6705
    it("should offer a toCharArray method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.toCharArray shouldEqual
          regStr.value.toCharArray
      }
    }
    // SKIP-DOTTY-END
    it("should offer toLowerCase methods that are consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.toLowerCase shouldEqual
          regStr.value.toLowerCase

        // SKIP-SCALATESTJS,NATIVE-START
        // SKIP-DOTTY-START
        regStr.toLowerCase(Locale.getDefault) shouldEqual
          regStr.value.toLowerCase(Locale.getDefault)
        // SKIP-DOTTY-END  
        // SKIP-SCALATESTJS,NATIVE-END
      }
    }
    it("should offer toUpperCase methods that are consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.toUpperCase shouldEqual
          regStr.value.toUpperCase

        // SKIP-SCALATESTJS,NATIVE-START
        // SKIP-DOTTY-START
        regStr.toUpperCase(Locale.getDefault) shouldEqual
          regStr.value.toUpperCase(Locale.getDefault)
        // SKIP-DOTTY-END
        // SKIP-SCALATESTJS,NATIVE-END
      }
    }
    // SKIP-DOTTY-START
    it("should offer a trim method that is consistent with String") {
      forAll { (regStr: RegexString) =>
        regStr.trim shouldEqual
          regStr.value.trim
      }
    }
    // SKIP-DOTTY-END
  }
}

