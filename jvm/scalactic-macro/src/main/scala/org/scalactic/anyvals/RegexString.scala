/*
* Copyright 2001-2025 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import java.nio.charset.Charset
// SKIP-SCALATESTJS,NATIVE-START
import java.util.Locale
// SKIP-SCALATESTJS,NATIVE-END

private[scalactic] final class RegexString private (val value: String) extends AnyVal {
  override def toString: String = s"RegexString($value)"

  def length: Int = value.length

  def charAt(index: Int): Char = value.charAt(index)

  def codePointAt(index: Int): Int = value.codePointAt(index)

  // SKIP-SCALATESTJS,NATIVE-START
  def codePointBefore(index: Int): Int = value.codePointBefore(index)
  // SKIP-SCALATESTJS,NATIVE-END

  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    value.codePointCount(beginIndex, endIndex)

  def compareTo(anotherString: String): Int =
    value.compareTo(anotherString)

  def compareToIgnoreCase(anotherString: String): Int =
    value.compareToIgnoreCase(anotherString)

  def concat(str: String): String =
    value.concat(str)

  def contains(s: CharSequence): Boolean =
    value.contains(s)

  // SKIP-SCALATESTJS,NATIVE-START
  def contentEquals(cs: CharSequence): Boolean =
    value.contentEquals(cs)
  // SKIP-SCALATESTJS,NATIVE-END

  def endsWith(suffix: String): Boolean =
    value.endsWith(suffix)

  def getBytes: Array[Byte] =
    value.getBytes

  def getBytes(charset: Charset): Array[Byte] =
    value.getBytes(charset)

  def getBytes(charsetName: String): Array[Byte] =
    value.getBytes(charsetName)

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
    value.getChars(srcBegin, srcEnd, dst, dstBegin)

  def indexOf(ch: Int): Int =
    value.indexOf(ch)

  def indexOf(ch: Int, fromIndex: Int): Int =
    value.indexOf(ch, fromIndex)

  def indexOf(str: String): Int =
    value.indexOf(str)

  def indexOf(str: String, fromIndex: Int): Int =
    value.indexOf(str, fromIndex)

  def intern: String =
    value.intern

  def isEmpty: Boolean =
    value.isEmpty

  def lastIndexOf(ch: Int): Int =
    value.lastIndexOf(ch)

  def lastIndexOf(ch: Int, fromIndex: Int): Int =
    value.lastIndexOf(ch, fromIndex)

  def lastIndexOf(str: String): Int =
    value.lastIndexOf(str)

  def lastIndexOf(str: String, fromIndex: Int): Int =
    value.lastIndexOf(str, fromIndex)

  def matches(regex: String): Boolean =
    value.matches(regex)

  // SKIP-SCALATESTJS,NATIVE-START
  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    value.offsetByCodePoints(index, codePointOffset)
  // SKIP-SCALATESTJS,NATIVE-END

  def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
    value.regionMatches(ignoreCase, toffset, other, ooffset, len)

  def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
    value.regionMatches(toffset, other, ooffset, len)

  def replace(oldChar: Char, newChar: Char): String =
    value.replace(oldChar, newChar)

  def replace(target: CharSequence, replacement: CharSequence): String =
    value.replace(target, replacement)

  def replaceAll(regex: String, replacement: String): String =
    value.replaceAll(regex, replacement)

  def replaceFirst(regex: String, replacement: String): String =
    value.replaceFirst(regex, replacement)

  def split(regex: String): Array[String] =
    value.split(regex)

  def split(regex: String, limit: Int): Array[String] =
    value.split(regex, limit)

  def startsWith(prefix: String): Boolean =
    value.startsWith(prefix)

  def startsWith(prefix: String, toffset: Int): Boolean =
    value.startsWith(prefix, toffset)

  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    value.subSequence(beginIndex, endIndex)

  def substring(beginIndex: Int): String =
    value.substring(beginIndex)

  def substring(beginIndex: Int, endIndex: Int): String =
    value.substring(beginIndex, endIndex)

  def toCharArray: Array[Char] =
    value.toCharArray

  def toLowerCase: String =
    value.toLowerCase

// SKIP-SCALATESTJS,NATIVE-START
  def toLowerCase(locale: Locale): String =
    value.toLowerCase(locale: Locale)
// SKIP-SCALATESTJS,NATIVE-END

  def toUpperCase: String =
    value.toUpperCase

// SKIP-SCALATESTJS,NATIVE-START
  def toUpperCase(locale: Locale): String =
    value.toUpperCase(locale: Locale)
// SKIP-SCALATESTJS,NATIVE-END

  def trim: String =
    value.trim

  def ensuringValid(f: String => String): RegexString = {
    val candidateResult: String = f(value)
    if (RegexStringMacro.isValid(candidateResult)) new RegexString(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid RegexString")
  }
}

private[scalactic] object RegexString {
  def from(value: String): Option[RegexString] =
    if (RegexStringMacro.isValid(value)) Some(new RegexString(value)) else None

  def ensuringValid(value: String): RegexString =
    if (RegexStringMacro.isValid(value)) new RegexString(value) else {
      throw new AssertionError(s"$value was not a valid RegexString")
    }

  def isValid(value: String): Boolean = RegexStringMacro.isValid(value)

  def fromOrElse(value: String, default: => RegexString): RegexString =
    if (RegexStringMacro.isValid(value)) new RegexString(value) else default

  import scala.language.experimental.macros
  def apply(value: String): RegexString = macro RegexStringMacro.apply
}

