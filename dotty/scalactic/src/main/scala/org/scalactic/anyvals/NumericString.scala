/*
* Copyright 2001-2024 Artima, Inc.
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
import java.util.Locale
import scala.collection.immutable.StringOps
import scala.collection.immutable.WrappedString
import scala.collection.mutable.Buffer
import scala.collection.GenSeq
import scala.collection.SeqView
import scala.collection.GenIterable
import scala.collection.generic.CanBuildFrom
//import scala.collection.parallel.ParSeq
import scala.util.matching.Regex
import scala.language.higherKinds
import scala.util.{Try, Success, Failure}
import org.scalactic.{Validation, Pass, Fail}
import org.scalactic.{Or, Good, Bad}

/**
 * An <code>AnyVal</code> for numeric <code>String</code>s.
 *
 * Note: a <code>NumericString</code> contains only numeric digit characters.
 *
 * <p>
 * Because <code>NumericString</code> is an <code>AnyVal</code>
 * it will usually be as efficient as a <code>String</code>, being
 * boxed only when a <code>String</code> would have been boxed.
 * </p>
 *
 * <p>
 * The <code>NumericString.apply</code> factory method is implemented in
 * terms of a macro that checks literals for validity at compile time. Calling
 * <code>NumericString.apply</code> with a literal <code>String</code> value
 * will either produce a valid <code>NumericString</code> instance at run
 * time or an error at compile time. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import anyvals._
 * import anyvals._
 *
 * scala&gt; NumericString("42")
 * res0: org.scalactic.anyvals.NumericString = NumericString(42)
 *
 * scala&gt; NumericString("abc")
 * &lt;console&gt;:11: error: NumericString.apply can only be invoked on String literals that contain numeric characters, i.e., decimal digits '0' through '9', like "123".
 *               NumericString("abc")
 *                            ^
 * </pre>
 *
 * <p>
 * <code>NumericString.apply</code> cannot be used if the value being passed
 * is a variable (<em>i.e.</em>, not a literal), because the macro cannot
 * determine the validity of variables at compile time (just literals). If
 * you try to pass a variable to <code>NumericString.apply</code>, you'll
 * get a compiler error that suggests you use a different factory method,
 * <code>NumericString.from</code>, instead:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; val x = "1"
 * x: String = 1
 *
 * scala&gt; NumericString(x)
 * &lt;console&gt;:15: error: NumericString.apply can only be invoked on String literals that contain only numeric characters, i.e., decimal digits '0' through '9', like "123" Please use NumericString.from instead.
 *               NumericString(x)
 *                            ^
 * </pre>
 *
 * <p>
 * The <code>NumericString.from</code> factory method will inspect the value
 * at runtime and return an <code>Option[NumericString]</code>. If
 * the value is valid, <code>NumericString.from</code> will return a
 * <code>Some[NumericString]</code>, else it will return a <code>None</code>.
 * Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; NumericString.from(x)
 * res3: Option[org.scalactic.anyvals.NumericString] = Some(NumericString(1))
 *
 * scala&gt; val y = "a"
 * y: String = a
 *
 * scala&gt; NumericString.from(y)
 * res4: Option[org.scalactic.anyvals.NumericString] = None
 * </pre>
 *
 * @param value The <code>String</code> value underlying this
 *              <code>NumericString</code>.
 */
final class NumericString private (val value: String) extends AnyVal {

  /**
   * A string representation of this <code>NumericString</code>.
   */
  override def toString: String = s"NumericString($value)"

  /**
   * Returns length of this `NumericString` in Unicode characters.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      length of this `NumericString`
   */
  def length: Int = value.length

  /**
   * Returns the character at the zero-based `index` within the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param index zero-based offset within `NumericString`
   * @return      character found at `index`
   */
  def charAt(index: Int): Char = value.charAt(index)

  /**
   * Returns the integer value of the Unicode code point at the zero-based `index` within the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param index zero-based offset within `NumericString`
   * @return      Unicode code point found at 'index'
   */
  def codePointAt(index: Int): Int = value.codePointAt(index)

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Returns the integer value of the Unicode code point immediately prior to the zero-based `index` within the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param index zero-based offset within `NumericString`
   * @return      Unicode code point found immediately prior to 'index'
   */
  def codePointBefore(index: Int): Int = value.codePointBefore(index)
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Returns the count of complete Unicode code points beginning at zero-based `beginIndex` and ending at (but not including) zero-based `endIndex`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param beginIndex  zero-based starting offset within `NumericString`
   * @param endIndex    zero-based ending offset within `NumericString`, one-past character range of interest
   * @return            count of complete Unicode code points found from BeginIndex, up to endIndex
   */
  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    value.codePointCount(beginIndex, endIndex)

  /**
   * Compares the `NumericString` to `anotherString`, returning an integer <0 if `NumericString` is less than the supplied string,
   * 0 if identical, and >0 if `NumericString` is greater than the supplied string.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param anotherString  other string we compare `NumericString` against
   * @return               integer <0, 0 or >0 corresponding to lexicographic ordering of `NumericString` vs. `anotherString`
   */
  def compareTo(anotherString: String): Int =
    value.compareTo(anotherString)

  // We'll leave this one out because numeric characters are the same upper and lower case.
  /**
   * Compares the `NumericString` to `anotherString` ignoring case, returning an integer <0 if `NumericString` is less than the supplied string,
   * 0 if identical, and >0 if `NumericString` is greater than the supplied string.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param anotherString    other string we compare `NumericString` against
   * @return                 integer <0, 0 or >0 corresponding to case-insensitive lexicographic ordering
   *                         of `NumericString` vs. `anotherString`
  def compareToIgnoreCase(anotherString: String): Int =
    value.compareToIgnoreCase(anotherString)
   */

  /**
   * Concatenates supplied string `str` onto the tail end of the `NumericString` and returns the resulting String.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param str   additional string to concatenate onto `NumericString`
   * @return      string resulting from the concatenation
   */
  def concat(str: String): String =
    value.concat(str)

  /**
   * Tests whether this `NumericString` contains a given value as an element.
   *
   *  @param s    the element to test.
   *  @return     `true` if this `NumericString` has an element that
   *              is equal (as determined by `==`) to `elem`,
   *              `false` otherwise.
   */
  def contains(s: CharSequence): Boolean =
    value.contains(s)

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * Returns `true` if the `NumericString` content is the same as the supplied character sequence `cs`, otherwise returns `false`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param cs    character sequence for comparison
   * @return      `true` if `NumericString` content is the same as `cs`
   *              `false` otherwise.
   */
  def contentEquals(cs: CharSequence): Boolean =
    value.contentEquals(cs)
  // SKIP-SCALATESTJS,NATIVE-END

  // We are missing contentEquals(StringBuffer)

  /**
   * Returns `true` if the `NumericString` content completely matches the supplied `suffix`, when both strings are aligned at their endpoints.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param suffix    string for comparison as a suffix
   * @return          `true` if `NumericString` content completely matches the supplied `suffix`
   *                  `false` otherwise.
   */
  def endsWith(suffix: String): Boolean =
    value.endsWith(suffix)

  /**
   * Returns an array of bytes corresponding to the `NumericString` characters, interpreted via the platform's default Charset-mapping of
   * Unicode code points.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      array of bytes corresponding to `NumericString` characters
   */
  def getBytes: Array[Byte] =
    value.getBytes

  /**
   * Returns an array of bytes corresponding to the `NumericString` characters, interpreted via the supplied `charset` mapping of Unicode code points.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param charset    a mapping of Unicode code points to bytes
   * @return           array of bytes corresponding to `NumericString` characters
   */
  def getBytes(charset: Charset): Array[Byte] =
    value.getBytes(charset)

  /**
   * Returns an array of bytes corresponding to the `NumericString` characters, interpreted via the named `charsetName` Charset-mapping of Unicode
   * code points.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param charsetName    string that names an already-known mapping of Unicode code points to bytes
   * @return               array of bytes corresponding to `NumericString` characters
   */
  def getBytes(charsetName: String): Array[Byte] =
    value.getBytes(charsetName)

  /**
   * Extracts the range of `NumericString` characters beginning at zero-based `srcBegin` through but not including `srcEnd`,
   * into the supplied character array `dst`, writing the characters at the zero-based `dstBegin` index forward within that array.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param srcBegin    zero-based index where to begin extracting characters from `NumericString`
   * @param srcEnd      zero-based limit before which to stop extracting characters from `NumericString`
   * @param dst         supplied character array to write extracted characters into
   * @param dstBegin    zero-based index within destination array at which to begin writing
   * @return            Unit -- this function modifies the supplied `dst` array
   */
  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int): Unit =
    value.getChars(srcBegin, srcEnd, dst, dstBegin)

  /**
   * Returns zero-based index in Unicode code units (logical index of characters) of first-encountered `NumericString` character
   * matching the supplied Unicode `ch`; returns -1 if no matching character is found.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param ch    Unicode character to look for
   * @return      zero-based integer index in Unicode code units of first-encountered instance of `ch`
   */
  def indexOf(ch: Int): Int =
    value.indexOf(ch)

  /**
   * Returns zero-based index (in Unicode code units: logical index of characters) of first-encountered `NumericString` character
   * matching supplied Unicode `ch`, beginning search at zero-based index `fromIndex`; returns -1 if no matching character is found
   * or if `fromIndex` is outside the bounds of `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param ch           Unicode character to look for
   * @param fromIndex    zero-based integer index at which to begin search for match of `ch` character
   * @return             zero-based integer index in Unicode code units of first-encountered instance of `ch` at/beyond `fromIndex`
   */
  def indexOf(ch: Int, fromIndex: Int): Int =
    value.indexOf(ch, fromIndex)

  /**
   * Returns zero-based index (in Unicode code units: logical index of characters) of starting-character position
   * of first-encountered match of `str` within `NumericString`;
   * returns -1 if no fully-matching substring is found.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param str          Unicode string to look for
   * @return             zero-based integer index in Unicode code units,
   *                     of starting position of first-encountered instance of `str` in `NumericString`
   */
  def indexOf(str: String): Int =
    value.indexOf(str)

  /**
   * Returns zero-based index (in Unicode code units: logical index of characters) of starting-character position
   * of first-encountered match of `str` within `NumericString`, beginning search at zero-based index `fromIndex`;
   * returns -1 if no fully-matching substring is found, or if `fromIndex` is outside the bounds of `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param str          Unicode string to look for
   * @param fromIndex    zero-based integer index at which to begin search for match of `str` string
   * @return             zero-based integer index in Unicode code units, of starting position of
   *                     first-encountered instance of `str` in `NumericString` at/beyond `fromIndex`
   */
  def indexOf(str: String, fromIndex: Int): Int =
    value.indexOf(str, fromIndex)

  /**
   * Add this immutable `NumericString`'s `String` value to the pool of interned strings,
   * so there is only one copy of the string's representation in memory, shared among all instances.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      String which is now in the pool of interned strings
   */
  def intern: String =
    value.intern

  /**
   * Returns `true` if `NumericString` contains no characters (not even whitespace); otherwise returns `false`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      `true` if `NumericString` contains no characters (not even whitespace)
   *              `false` otherwise.
   */
  def isEmpty: Boolean =
    value.isEmpty

  /**
   * Returns zero-based index of the final occurrence of the Unicode character `ch` in the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param ch    Unicode character for which to search backwards
   * @return      zero-based integer index of the final occurrence of this character in `NumericString`;
   *              -1 if not found
   */
  def lastIndexOf(ch: Int): Int =
    value.lastIndexOf(ch)

  /**
   * Returns zero-based index of the final occurrence of the Unicode character `ch` in the `NumericString`, with search beginning
   * at zero-based `fromIndex` and proceeding backwards.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param ch           Unicode character for which to search backwards
   * @param fromIndex    zero-based index of starting position
   * @return             zero-based index of the final (rightmost) occurrence of this character in `NumericString`;
   *                     -1 if not found
   */
  def lastIndexOf(ch: Int, fromIndex: Int): Int =
    value.lastIndexOf(ch, fromIndex)

  /**
   * Returns zero-based index from the beginning of `NumericString` of the first character position for where the string `str`
   * fully matched rightmost within `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param str    string for comparison
   * @return      zero-based integer index of first character position of where `str` fully matched rightmost within `NumericString`;
   *              -1 if not found
   */
  def lastIndexOf(str: String): Int =
    value.lastIndexOf(str)

  /**
   * Returns zero-based index from the beginning of `NumericString` of the first character position for where the string `str`
   * fully matched rightmost within `NumericString`, with search beginning at zero-based `fromIndex` and proceeding backwards.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param str          string for comparison
   * @param fromIndex    zero-based index of starting position
   * @return             zero-based integer index of first character position of where `str` fully matched rightmost within `NumericString`;
   *                     -1 if not found
   */
  def lastIndexOf(str: String, fromIndex: Int): Int =
    value.lastIndexOf(str, fromIndex)

  /**
   * Returns `true` if the this `NumericString`'s `String` value matches the supplied regular expression, `regex`; otherwise returns `false`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param regex    regular-expression string
   * @return         `true` if `NumericString` content matches supplied regular expression `regex`;
   *                 `false` otherwise.
   */
  def matches(regex: String): Boolean =
    value.matches(regex)

  // SKIP-SCALATESTJS,NATIVE-START
  // This method returns the index of the character that is sitting at the position
  // computed by starting at index, and hopping over to the right codePointOffset code points.

  /**
   * Returns the "byte distance" required from start of string,
   * to reach the position of the supplied byte `index` plus the number of `codePointOffset` points further.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param index              byte index of start position in spacing computation
   * @param codePointOffset    how many code points to advance (may be variable length per code point)
   * @return                   zero-based offset in bytes from start of `NumericString`, to reach the designated position
   */
  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    value.offsetByCodePoints(index, codePointOffset)
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Returns `true` if the given region of text matches completely for the `len` characters beginning at `toffset` in the `NumericString` text
   * and at `ooffset` in the supplied `other` string text, with the option to `ignoreCase` during matching; otherwise returns `false`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param ignoreCase    if nonzero, comparison ignores case
   * @param toffset       zero-based offset of start point of comparison in `NumericString` text
   * @param other         string supplied for comparison
   * @param ooffset       zero-based offset of start point of comparison in `other` string text
   * @param len           length of comparison, in characters
   * @return              `true` if region of text matches completely for given length;
   *                      `false` otherwise.
   */
  def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
    value.regionMatches(ignoreCase, toffset, other, ooffset, len)

  /**
   * Returns `true` if the given region of text matches completely for the `len` characters beginning at `toffset` in the `NumericString` text
   * and at `ooffset` in the supplied `other` string text; otherwise returns `false`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param toffset       zero-based offset of start point of comparison in `NumericString` text
   * @param other         string supplied for comparison
   * @param ooffset       zero-based offset of start point of comparison in `other` string text
   * @param len           length of comparison, in characters
   * @return              `true` if region of text matches completely for given length;
   *                      `false` otherwise.
   */
  def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean =
    value.regionMatches(toffset, other, ooffset, len)

  /**
   * Returns the new `String` resulting from replacing all occurrences of `oldChar` with `newChar`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param oldChar    character to replace
   * @param newChar    character that will take its place
   * @return           string resulting from zero or more replacement(s)
   */
  def replace(oldChar: Char, newChar: Char): String =
    value.replace(oldChar, newChar)

  /**
   * Returns the new `String` resulting from replacing all occurrences of `CharSequence` `target` with `replacement`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param target         character sequence to replace
   * @param replacement    character sequence that will take its place
   * @return               string resulting from zero or more replacement(s)
   */
  def replace(target: CharSequence, replacement: CharSequence): String =
    value.replace(target, replacement)

  /**
   * Returns the new `String` resulting from replacing all `regex` string matches with the `replacement` string.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param regex          regular expression string
   * @param replacement    string to replace in place of any regular expression matches in the original `NumericString`
   * @return               string resulting from zero or more replacement(s)
   */
  def replaceAll(regex: String, replacement: String): String =
    value.replaceAll(regex, replacement)

  /**
   * Returns the new `String` resulting from replacing the first-found `regex` string match with the `replacement` string.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param regex          regular expression string
   * @param replacement    string to replace in place of first regular expression match in the original `NumericString`
   * @return               string resulting from zero or one replacement(s)
   */
  def replaceFirst(regex: String, replacement: String): String =
    value.replaceFirst(regex, replacement)

  /**
   * Returns an array of strings produced by splitting `NumericString` at every location matching the supplied `regex`;
   * the `regex`-matching characters are omitted from the output.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param regex    string for pattern matching
   * @return         array of strings produced by splitting `NumericString` at every location matching supplied `regex`
   */
  def split(regex: String): Array[String] =
    value.split(regex)

  /**
   * Returns an array of strings produced by splitting `NumericString` at up to `limit` locations matching the supplied `regex`;
   * the `regex`-matching characters are omitted from the output.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param regex    string for pattern matching
   * @param limit    maximum number of split output strings that will be generated by this function; further potential splits get ignored
   * @return         array of strings produced by splitting `NumericString` at every location matching supplied `regex`, up to `limit` occurrences
   */
  def split(regex: String, limit: Int): Array[String] =
    value.split(regex, limit)

  /**
   * Returns `true` if the `NumericString` content completely matches the supplied `prefix`, when both strings are aligned at their startpoints
   * up to the length of `prefix`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param prefix    string for comparison as prefix
   * @return          `true` if `NumericString` content is the same as `prefix` for the entire length of `prefix`
   *                  `false` otherwise.
   */
  def startsWith(prefix: String): Boolean =
    value.startsWith(prefix)

  /**
   * Returns `true` if the `NumericString` content completely matches the supplied `prefix`, starting at `toffset` characters in the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param prefix    string for comparison as prefix
   * @param toffset   zero-based integer start point for comparison within the `NumericString`
   * @return          `true` if `NumericString` content is the same as `prefix` for the entire length of `prefix` shifted over,
   *                  `false` otherwise.
   */
  def startsWith(prefix: String, toffset: Int): Boolean =
    value.startsWith(prefix, toffset)

  /**
   * Returns the character sequence extracted from `NumericString` beginning at zero-based offset `beginIndex`
   * and continuing until (but not including) offset `endIndex`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param beginIndex    zero-based integer offset at which character extraction begins
   * @param endIndex      zero-based integer offset before which character extraction ends
   * @return              CharSequence of zero or more extracted characters
   */
  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    value.subSequence(beginIndex, endIndex)

  /**
   * Returns a string extracted from `NumericString` from the zero-based `beginIndex` through the end of the string.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param beginIndex    zero-based integer offset at which substring extraction begins (continues through end of `NumericIndex` string)
   * @return              returns string extracted from `NumericString`
   */
  def substring(beginIndex: Int): String =
    value.substring(beginIndex)

  /**
   * Returns a string extracted `NumericString` starting from the zero-based `beginIndex` through but not including the zero-based `endIndex` offset.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param beginIndex    zero-based integer offset at which substring extraction begins
   * @param endIndex      zero-based integer offset before which substring extraction ends
   * @return              returns string extracted from `NumericString`
   */
  def substring(beginIndex: Int, endIndex: Int): String =
    value.substring(beginIndex, endIndex)

  /**
   * Returns an array of Unicode characters corresponding to the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      array of Unicode characters corresponding to the `NumericString`
   */
  def toCharArray: Array[Char] =
    value.toCharArray

  // SKIP-SCALATESTJS,NATIVE-START
  // These should stay because we'll use them in other String AnyVals, but
  // they don't make sense to invoke on NumericString because they would have no effect.
  // So I commmented them out. Please leave the comments here so we can use these as
  // a template for the other AnyVals which we'll add soon.
  /**
   * Returns the string resulting from converting any upper-case characters in `NumericString` into lower-case.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      string corresponding to the original string but with upper case characters converted to lower case
  def toLowerCase: String =
    value.toLowerCase
   */

  /**
   * Returns the string resulting from converting any upper-case characters in `NumericString` into lower-case
   * where case mapping is inferred from the `locale`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param locale    locale mapping to determine case equivalence
   * @return          string corresponding to the original string but with upper case characters converted to lower case

  def toLowerCase(locale: Locale): String =
    value.toLowerCase(locale: Locale)
   */

  /**
   * Returns the string resulting from converting any lower-case characters in `NumericString` into upper-case.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      string corresponding to the original string but with lower case characters converted to upper case
  def toUpperCase: String =
    value.toUpperCase
   */

  /**
   * Returns the string resulting from converting any lower-case characters in `NumericString` into upper-case
   * where case mapping is inferred from the `locale`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param locale    locale mapping to determine case equivalence
   * @return          string corresponding to the original string but with lower case characters converted to upper case

  def toUpperCase(locale: Locale): String =
    value.toUpperCase(locale: Locale)
   */
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * Return new string resulting from removing any whitespace characters from the start and end of the `NumericString`.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @return      string resulting from removing any whitespace characters from start and end of original string
   */
  def trim: String =
    value.trim

  /**
   * Returns a new <code>NumericString</code> concatenating this
   * <code>NumericString</code> with the passed <code>NumericString</code>.
   *
   * @param that the <code>NumericString</code> to append
   * @return a new <code>NumericString</code> that concatenates
   *   this <code>NumericString</code> with <code>that</code>.
   */
  def concatNumericString(that: NumericString): NumericString =
    new NumericString(value ++ that.value)

  /**
   * Return a <code>NumericString</code> consisting of the current
   * <code>NumericString</code> concatenated <code>n</code> times.
   */
  def *(n: Int): NumericString =
    new NumericString(value * n)

  /**
   * Returns a new <code>String</code> concatenating this
   * <code>NumericString</code> with the passed <code>String</code>.
   *
   * @param that the <code>String</code> to append
   * @return a new `String` which contains all elements of this
   *         `NumericString` followed by all elements of that
   */
  def ++(that: String): String =
    value ++ that

  /**
   * Returns a new <code>String</code> consisting of this
   * <code>NumericString</code> prepended by the passed
   * <code>String</code>.
   *
   * @param that the <code>String</code> to append
   * @return a new <code>String</code> which contains all elements of that
   *   followed by all elements of this <code>NumericString</code>
   */
  def ++:(that: String): String =
    that ++: value

  /**
   * Returns a new <code>String</code> consisting of this
   * <code>NumericString</code> prepended by the passed
   * <code>Char</code>.
   *
   * @param elem the prepended `Char`
   * @return a new <code>String</code> consisting of `elem`
   *         followed by all characters from this <code>NumericString</code>
   */
  def +:(elem: Char): String =
    elem +: value

  /**
   * Applies a binary operator to a start value and all elements
   * of this `NumericString`, going left to right.
   *
   * Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the
   * same as `xs foldLeft z`.
   *
   * @param z the start value
   * @param op the binary operator
   * @return the result of inserting `op` between consecutive
   *         `Chars` of this `NumericString`, going left to right with the
   *         start value `z` on the left:
   *           {{{
   *             op(...op(op(z, x_1), x_2), ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the characters of this
   *           `NumericString`.
   */
  def /:(z: Int)(op: (Int, Char) => Int): Int =
    (z /: value)(op)

  /**
   * Returns a new `String` consisting of this `NumericString` with the
   * passed `Char` appended.
   *
   * @param elem the appended `Char`
   * @return a new `String` consisting of all elements of this `NumericString`
   *         followed by `elem`
   */
  def :+(elem: Char): String =
    value :+ elem

  /**
   * Applies a binary operator to all elements of this
   * `NumericString` and a start value, going right to
   * left.
   *
   * Note: :\ is alternate syntax for foldRight; xs :\ z is the
   * same as xs foldRight z.
   *
   * @param z the start value
   * @param op the binary operator
   * @return the result of inserting `op` between consecutive
   *         characters of this `NumericString`, going right to left with the
   *         start value `z` on the right:
   *         {{{
   *           op(x_1, op(x_2, ... op(x_n, z)...))
   *         }}}
   *         where `x,,1,,, ..., x,,n,,` are the characters of this
   *         `NumericString`.
   */
  def :\(z: Int)(op: (Char, Int) => Int): Int =
    (value :\ z)(op)

  /**
   * Returns true if `this` is less than `that`
   */
  def <(that: String): Boolean =
    (value < that)

  /**
   * Returns true if `this` is less than or equal to `that`
   */
  def <=(that: String): Boolean =
    (value <= that)

  /**
   * Returns true if `this` is greater than `that`
   */
  def >(that: String): Boolean =
    (value > that)

  /**
   * Returns true if `this` is greater than or equal to `that`
   */
  def >=(that: String): Boolean =
    (value >= that)

  /**
   * Appends string value of this `NumericString` to a string builder.
   *
   * @param b the string builder to which this `NumericString` gets appended
   * @return the string builder b to which this `NumericString` was appended
   */
  def addString(b: StringBuilder): StringBuilder =
    value.addString(b)

  /**
   * Appends character elements of this `NumericString` to a string builder
   * using a separator string.
   *
   * @param b the string builder to which elements are appended
   * @param sep the separator string
   * @return the string builder b to which elements were appended
   */
  def addString(b: StringBuilder, sep: String): StringBuilder =
    value.addString(b, sep)

  /**
   * Appends character elements of this `NumericString` to a
   * string builder using start, separator, and end strings. The
   * written text begins with the string `start` and ends with the
   * string `end`. Inside, the characters of this `NumericString`
   * are separated by the string `sep`.
   *
   * @param b the string builder to which elements are appended
   * @param start the starting string
   * @param sep the separator string
   * @param end the ending string
   * @return the string builder b to which elements were appended
   */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    value.addString(b, start, sep, end)

  /**
   * Aggregates the results of applying an operator to subsequent elements
   * of this `NumericString`.
   *
   * This is a more general form of `fold` and `reduce`. It is
   * similar to `foldLeft` in that it doesn't require the result
   * to be a supertype of the element type.
   *
   * `aggregate` splits the elements of this `NumericString` into
   * partitions and processes each partition by sequentially
   * applying `seqop`, starting with `z` (like `foldLeft`). Those
   * intermediate results are then combined by using `combop`
   * (like `fold`). The implementation of this operation may
   * operate on an arbitrary number of collection partitions
   * (even 1), so `combop` may be invoked an arbitrary number of
   * times (even 0).
   *
   * As an example, consider summing up the integer values the
   * character elements.  The initial value for the sum is
   * 0. First, `seqop` transforms each input character to an Int
   * and adds it to the sum (of the partition). Then, `combop`
   * just needs to sum up the intermediate results of the
   * partitions:
   *  {{{
   *    NumericString("123").aggregate(0)({ (sum, ch) => sum + ch.toInt }, { (p1, p2) => p1 + p2 })
   *  }}}
   *
   * @param B the type of accumulated results
   * @param z the initial value for the accumulated result of the partition -
   *          this will typically be the neutral element for the
   *          `seqop` operator (e.g.  `Nil` for list
   *          concatenation or `0` for summation) and may be
   *          evaluated more than once
   * @param seqop an operator used to accumulate results within a partition
   * @param combop an associative operator used to combine results within a
   *               partition
   */
  def aggregate[B](z: => B)(seqop: (B, Char) => B, combop: (B, B) => B): B =
    value.aggregate[B](z)(seqop, combop)

  /**
   * Return character at index `index`.
   *
   * @return the character of this string at index `index`, where
   *         `0` indicates the first element.
   */
  def apply(index: Int): Char =
    value(index)

  /**
   * Method called from equality methods, so that user-defined
   * subclasses can refuse to be equal to other collections of
   * the same kind.
   *
   * @param that the object with which this `NumericString` should be compared
   * @return `true` if this `NumericString` can possibly equal
   *         `that`, `false` otherwise. The test takes into
   *         consideration only the run-time types of objects but
   *         ignores their elements.
   */
  def canEqual(that: Any): Boolean =
    that.isInstanceOf[NumericString]

  /**
   * Returns this string with first character converted to upper case (i.e.
   * unchanged for a `NumericString`).
   *
   * @return the string value of this `NumericString`.
   */
  def capitalize: String =
    value.capitalize

/*
  def chars: java.util.stream.IntStream =
    value.chars

  def codePoints: java.util.stream.IntStream =
    value.codePoints
*/

  /**
   * Builds a new collection by applying a partial function to
   *  all characters of this `NumericString` on which the function
   *  is defined.
   *
   *  @param pf     the partial function which filters and maps the elements.
   *  @tparam B     the element type of the returned collection.
   *  @return a new String resulting from applying the partial
   *          function `pf` to each element on which it is
   *          defined and collecting the results.  The
   *          order of the elements is preserved.
   */
  def collect[B](pf: PartialFunction[Char, B]): String =
    value.collect(pf).mkString

  /**
   * Finds the first character of the `NumericString` for
   * which the given partial function is defined, and applies
   * the partial function to it.
   *
   * @param pf   the partial function
   * @return     an option value containing pf applied to the first
   *             value for which it is defined, or `None` if none exists.
   */
  def collectFirst[B](pf: PartialFunction[Char, B]): Option[B] =
    value.collectFirst(pf)

  /**
   * Iterates over combinations.  A _combination_ of length `n`
   * is a subsequence of the original sequence, with the
   * elements taken in order.  Thus, `"xy"` and `"yy"` are both
   * length-2 combinations of `"xyy"`, but `"yx"` is not.  If
   * there is more than one way to generate the same
   * subsequence, only one will be returned.
   *
   * For example, `"xyyy"` has three different ways to generate
   * `"xy"` depending on whether the first, second, or third
   * `"y"` is selected.  However, since all are identical, only
   * one will be chosen.  Which of the three will be taken is an
   * implementation detail that is not defined.
   *
   *  @return An Iterator which traverses the possible n-element
   *  combinations of this `NumericString`.
   *  @example  `NumericString("12223").combinations(2) = Iterator(12, 13, 22, 23)`
   */
  def combinations(n: Int): Iterator[String] =
    value.combinations(n)

  /**
   * Result of comparing `this` with operand `that`.
   *
   * Implement this method to determine how instances of A will be sorted.
   *
   * Returns `x` where:
   *
   *   - `x < 0` when `this < that`
   *
   *   - `x == 0` when `this == that`
   *
   *   - `x > 0` when  `this > that`
   *
   */
  def compare(that: String): Int =
    value.compare(that)

  /**
   * Tests whether this `NumericString` contains a given sequence as a slice.
   *
   * @param  that    the sequence to test
   * @return `true` if this `NumericString` contains a slice
   *          with the same elements as `that`, otherwise
   *          `false`.
   */
  def containsSlice[B](that: GenSeq[B]): Boolean =
    value.containsSlice(that)

  /** Copies the elements of this `NumericString` to an array.
   *  Fills the given array `xs` with at most `len` elements of
   *  this `NumericString`, starting at position `start`.
   *  Copying will stop once either the end of the current `NumericString` is reached,
   *  or the end of the target array is reached, or `len` elements have been copied.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   */
  def copyToArray(xs: Array[Char], start: Int, len: Int): Unit =
    value.copyToArray(xs, start, len)

  /** Copies the elements of this `NumericString` to an array.
   *  Fills the given array `xs` with values of this `NumericString`
   *  Copying will stop once either the end of the current
   *  `NumericString` is reached, or the end of the target array
   *  is reached.
   *
   *  @param  xs     the array to fill.
   */
  def copyToArray(xs: Array[Char]): Unit =
    value.copyToArray(xs)

  /** Copies the elements of this `NumericString` to an array.
   *  Fills the given array `xs` with values of this
   *  `NumericString`, beginning at index `start`.  Copying will
   *  stop once either the end of the current `NumericString` is
   *  reached, or the end of the target array is reached.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   */
  def copyToArray(xs: Array[Char], start: Int): Unit =
    value.copyToArray(xs, start)

  /** Copies all elements of this `NumericString` to a buffer.
   *  @param  dest   The buffer to which elements are copied.
   */
  def copyToBuffer[B >: Char](dest: Buffer[B]): Unit =
    value.copyToBuffer(dest)

  /** Tests whether every element of this `NumericString` relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param p the test predicate, which relates elements from
   *           both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return `true` if both sequences have the same length and
   *          `p(x, y)` is `true` for all corresponding
   *          elements `x` of this `NumericString` and
   *          `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: GenSeq[B])(p: (Char, B) => Boolean): Boolean =
    value.corresponds(that)(p)

  /** Counts the number of elements in the `NumericString` which satisfy a predicate.
   *
   *  @param p     the predicate  used to test elements.
   *  @return      the number of elements satisfying the predicate `p`.
   */
  def count(p: (Char) => Boolean): Int =
    value.count(p)

  /** Computes the multiset difference between this
   * `NumericString` and another sequence.
   *
   *  @param that   the sequence of elements to remove
   *  @return       a new string which contains all
   *                elements of this `NumericString` except some
   *                occurrences of elements that also appear
   *                in `that`.  If an element value `x` appears
   *                ''n'' times in `that`, then the first ''n''
   *                occurrences of `x` will not form part of the
   *                result, but any following occurrences will.
   */
  def diff(that: collection.Seq[Char]): String =
    value.diff(that)

  /** Builds a new `NumericString` from this `NumericString`
   * without any duplicate elements.
   *
   *  @return A new string which contains the first
   *          occurrence of every character of this `NumericString`.
   */
  def distinct: String =
    value.distinct

  /** Selects all elements except first ''n'' ones.
   *
   *  @param  n    the number of elements to drop from this `NumericString`.
   *  @return a string consisting of all elements of this
   *          `NumericString` except the first `n` ones, or else
   *          the empty string if this `NumericString` has less
   *          than `n` elements.
   */
  def drop(n: Int): String =
    value.drop(n)

  /** Selects all elements except last ''n'' ones.
   *
   *  @param  n    The number of elements to take
   *  @return a string consisting of all elements of this
   *          `NumericString` except the last `n` ones, or else
   *          the empty string, if this `NumericString` has less
   *          than `n` elements.
   */
  def dropRight(n: Int): String =
    value.dropRight(n)

  /** Drops longest prefix of elements that satisfy a predicate.
   *
   *  @param   p  The predicate used to test elements.
   *  @return  the longest suffix of this `NumericString` whose first element
   *           does not satisfy the predicate `p`.
   */
  def dropWhile(p: (Char) => Boolean): String =
    value.dropWhile(p)

  /** Tests whether this `NumericString` ends with the given sequence.
   *
   *  @param  that    the sequence to test
   *  @return `true` if this `NumericString` has `that` as a
   *          suffix, `false` otherwise.
   */
  def endsWith[B](that: GenSeq[B]): Boolean =
    value.endsWith(that)

  /** Returns `true` if the supplied `arg0` string is considered equal to this `NumericString`.
   *
   *  For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   *  @param arg0  string for comparison
   *  @return      `true` if `NumericString` content is the same as `arg0`
   *               `false` otherwise.
   */
  def equalsIgnoreCase(arg0: String): Boolean =
    value.equalsIgnoreCase(arg0)

  /** Tests whether a predicate holds for at least one element of this `NumericString`.
   *
   *  @param   p     the predicate used to test elements.
   *  @return `true` if the given predicate `p` is satisfied by
   *          at least one character of this `NumericString`, otherwise
   *          `false`
   */
  def exists(p: (Char) => Boolean): Boolean =
    value.exists(p)

  /** Selects all elements of this `NumericString` which satisfy a predicate.
   *
   *  @param p  the predicate used to test elements.
   *  @return a string consisting of all characters of this
   *          `NumericString` that satisfy the given
   *          predicate `p`. Their order may not be
   *          preserved.
   */
  def filter(p: (Char) => Boolean): String =
    value.filter(p)

  /** Selects all elements of this `NumericString` which do not
   * satisfy a predicate.
   *
   *  @param pred  the predicate used to test elements.
   *  @return a string consisting of all characters of this
   *          `NumericString` that do not satisfy the given
   *          predicate `p`. Their order may not be
   *          preserved.
   */
  def filterNot(p: (Char) => Boolean): String =
    value.filterNot(p)

  /** Finds the first element of the `NumericString` satisfying
   * a predicate, if any.
   *
   *  @param p       the predicate used to test elements.
   *  @return        an option value containing the first character of
   *                 the `NumericString` that satisfies `p`, or
   *                 `None` if none exists.
   */
  def find(p: (Char) => Boolean): Option[Char] =
    value.find(p)

  /** Builds a new collection by applying a function to all elements of this `NumericString`
   *  and using the elements of the resulting collections.
   *
   *  @tparam B     the element type of the returned collection.
   *  @param f      the function to apply to each element.
   *  @return       a new string resulting from applying the given
   *                  collection-valued function `f` to each
   *                  element of this `NumericString` and
   *                  concatenating the results.
   */
  def flatMap[B](f: (Char) => org.scalactic.ColCompatHelper.IterableOnce[B]): IndexedSeq[B] =
    value.flatMap(f)

  /** Folds the elements of this `NumericString` using the specified associative
   *  binary operator.
   *
   *  @tparam     A1 a type parameter for the binary operator, a
   *              supertype of `A`.
   *  @param z    a neutral element for the fold operation; may be
   *              added to the result an arbitrary number of
   *              times, and must not change the result
   *              (e.g., `Nil` for list concatenation, 0 for
   *              addition, or 1 for multiplication).
   *  @param op   a binary operator that must be associative.
   *  @return     the result of applying the fold operator `op`
   *              between all the elements and `z`, or `z` if this
   *              `NumericString` is empty.
   */
  def fold[A1 >: Char](z: A1)(op: (A1, A1) => A1): A1 =
    value.fold(z)(op)

  /** Applies a binary operator to a start value and all
   *  elements of this `NumericString`, going left to right.
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive
   *           elements of this `NumericString`, going left to
   *           right with the start value `z` on the left:
   *           {{{
   *             op(...op(z, x_1), x_2, ..., x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this
   *           `NumericString`. Returns `z` if this `NumericString` is empty.
   */
  def foldLeft[B](z: B)(op: (B, Char) => B): B =
    value.foldLeft(z)(op)

  /** Applies a binary operator to all elements of this
   *  `NumericString` and a start value, going right to left.
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.

   *  @return the result of inserting `op` between consecutive
   *           elements of this `NumericString`, going right to
   *           left with the start value `z` on the right:
   *           {{{
   *             op(x_1, op(x_2, ... op(x_n, z)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of
   *           this `NumericString`.  Returns `z` if this
   *           `NumericString` is empty.
   */
  def foldRight[B](z: B)(op: (Char, B) => B): B =
    value.foldRight(z)(op)

  /** Tests whether a predicate holds for all elements of this `NumericString`.
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if this `NumericString` is empty or the
   *                 given predicate `p` holds for all elements
   *                 of this `NumericString`, otherwise `false`.
   */
  def forall(p: (Char) => Boolean): Boolean =
    value.forall(p)

  /** Applies a function `f` to all elements of this `NumericString`.
    *
    *  @param f the function that is applied for its side-effect
    *           to every element.  The result of function `f`
    *           is discarded.
    */
  def foreach(f: (Char) => Unit): Unit =
    value.foreach(f)

  /** Partitions this `NumericString` into a map of strings
   * according to some discriminator function.
   *
   *  @param f     the discriminator function.
   *  @tparam K    the type of keys returned by the discriminator function.

   *  @return      A map from keys to strings such that the following
   *               invariant holds:
   *               {{{
   *                 (xs groupBy f)(k) = xs filter (x => f(x) == k)
   *               }}}
   *               That is, every key `k` is bound to a string of those
   *               elements `x` for which `f(x)` equals `k`.
   *
   */
  def groupBy[K](f: (Char) => K): Map[K, String] =
    value.groupBy(f)

  /** Partitions elements in fixed size strings.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing strings of size `size`,
   *          except the last will be less than size `size` if
   *          the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[String] =
    value.grouped(size)

  /** Tests whether this `NumericString` is known to have a finite size.
   *  Always `true` for `NumericString`.
   *
   *  @return  `true` if this collection is known to have finite size,
   *           `false` otherwise.
   */
  def hasDefiniteSize: Boolean =
    value.hasDefiniteSize

  /** Selects the first element of this `NumericString`.
   *
   *  @return  the first element of this `NumericString`.
   *  @throws NoSuchElementException if the `NumericString` is empty.
   */
  def head: Char =
    value.head

  /** Optionally selects the first element.
   *
   *  @return  the first element of this `NumericString` if it is nonempty,
   *           `None` if it is empty.
   */
  def headOption: Option[Char] =
    value.headOption

  /** Finds first index after or at a start index where this
   * `NumericString` contains a given sequence as a slice.
   *
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return         the first index `>= from` such that the elements
   *                   of this `NumericString` starting at this index
   *                   match the elements of sequence `that`, or `-1` of
   *                   no such subsequence exists.
   */
  def indexOfSlice[B >: Char](that: GenSeq[B], from: Int): Int =
    value.indexOfSlice(that, from)

  /** Finds first index where this `NumericString` contains a
   * given sequence as a slice.
   *
   *  @param  that    the sequence to test
   *  @return         the first index such that the elements of this
   *                  `NumericString` starting at this index match the
   *                  elements of sequence `that`, or `-1` of no such
   *                  subsequence exists.
   */
  def indexOfSlice[B >: Char](that: GenSeq[B]): Int =
    value.indexOfSlice(that)

  /** Finds index of the first element satisfying some predicate
   * after or at some start index.
   *
   *  @param   p      the predicate used to test elements.
   *  @param   from   the start index
   *  @return         the index `>= from` of the first element of this
   *                  `NumericString` that satisfies the predicate `p`,
   *                  or `-1`, if none exists.
   */
  def indexWhere(p: (Char) => Boolean, from: Int): Int =
    value.indexWhere(p, from)

  /** Finds index of first element satisfying some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @return        the index of the first element of this
   *                 `NumericString` that satisfies the predicate `p`,
   *                 or `-1`, if none exists.
   */
  def indexWhere(p: (Char) => Boolean): Int =
    value.indexWhere(p)

  /** Produces the range of all indices of this sequence.
   *
   *  @return  a `Range` value from `0` to one less than the
   *           length of this `NumericString`.
   */
  def indices: Range =
    value.indices

  /** Selects all elements except the last.
   *
   *  @return  a string consisting of all elements of this `NumericString`
   *           except the last one.
   *  @throws UnsupportedOperationException if the `NumericString` is empty.
   */
  def init: String =
    value.init

  /** Iterates over the inits of this `NumericString`. The first
   *  value will be the string for this `NumericString` and the
   *  final one will be an empty string, with the intervening
   *  values the results of successive applications of `init`.
   *
   *  @return  an iterator over all the inits of this `NumericString`
   *  @example  ` NumericString("123").inits = Iterator(123, 12, 1, "")`
   */
  def inits: Iterator[String] =
    value.inits

  /** Computes the multiset intersection between this
   * `NumericString` and another sequence.
   *
   *  @param that   the sequence of elements to intersect with.
   *  @return       a new string which contains all elements of this
   *                `NumericString` which also appear in `that`.  If an
   *                element value `x` appears ''n'' times in
   *                `that`, then the first ''n'' occurrences of
   *                `x` will be retained in the result, but any
   *                following occurrences will be omitted.
   *
   */
  def intersect(that: collection.Seq[Char]): String =
    value.intersect(that)


  /** Tests whether this `NumericString` contains given index.
   *
   * @param    idx     the index to test
   * @return   `true` if this `NumericString` contains an element
   *           at position `idx`, `false` otherwise.
   */
  def isDefinedAt(idx: Int): Boolean =
    value.isDefinedAt(idx)

  /** Tests whether this `NumericString` can be repeatedly traversed.  Always
   *  true for `NumericString`.
   *
   *  @return   `true` if it is repeatedly traversable, `false` otherwise.
   */
  final def isTraversableAgain: Boolean =
    value.isTraversableAgain

  /** Creates a new iterator over all elements contained in this
   * iterable object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[Char] =
    value.iterator

  /** Selects the last element.
    *
    * @return The last element of this `NumericString`.
    * @throws NoSuchElementException If the `NumericString` is empty.
    */
  def last: Char =
    value.last

  /** Finds last index before or at a given end index where this
   * `NumericString` contains a given sequence as a slice.
   *
   *  @param  that    the sequence to test
   *  @param  end     the end index
   *  @return    the last index `<= end` such that the elements of
   *             this `NumericString` starting at this index match
   *             the elements of sequence `that`, or `-1` of no
   *             such subsequence exists.
   */
  def lastIndexOfSlice[B >: Char](that: GenSeq[B], end: Int): Int =
    value.lastIndexOfSlice(that, end)

  /** Finds last index where this `NumericString` contains a
   * given sequence as a slice.
   *
   *  @param  that    the sequence to test
   *  @return    the last index such that the elements of this
   *              `NumericString` starting a this index match the
   *              elements of sequence `that`, or `-1` of no such
   *              subsequence exists.
   */
  def lastIndexOfSlice[B >: Char](that: GenSeq[B]): Int =
    value.lastIndexOfSlice(that)

  /** Finds index of last element satisfying some predicate
   * before or at given end index.
   *
   *  @param   p     the predicate used to test elements.
   *  @return    the index `<= end` of the last element of this
   *             `NumericString` that satisfies the predicate `p`,
   *             or `-1`, if none exists.
   */
  def lastIndexWhere(p: (Char) => Boolean, end: Int): Int =
    value.lastIndexWhere(p, end)

  /** Finds index of last element satisfying some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @return    the index of the last element of this
   *             `NumericString` that satisfies the predicate `p`,
   *             or `-1`, if none exists.
   */
  def lastIndexWhere(p: (Char) => Boolean): Int =
    value.lastIndexWhere(p)

  /** Optionally selects the last element.
   *
   *  @return  the last element of this `NumericString` if it is nonempty,
   *           `None` if it is empty.
   */
  def lastOption: Option[Char] =
    value.lastOption

  /** Compares the length of this `NumericString` to a test value.
   *
   *   @param   len   the test value that gets compared with the length.
   *   @return  A value `x` where
   *   {{{
   *        x <  0       if this.length <  len
   *        x == 0       if this.length == len
   *        x >  0       if this.length >  len
   *   }}}
   *  The method as implemented here does not call `length`
   *  directly; its running time is `O(length min len)` instead
   *  of `O(length)`. The method should be overwritten if
   *  computing `length` is cheap.
   */
  def lengthCompare(len: Int): Int =
    value.lengthCompare(len)

  /** Return all lines in this `NumericString` in an iterator.  Always
   * returns a single string for `NumericString`.
   */
  def lines: Iterator[String] =
    value.linesIterator

  /** Return all lines in this `NumericString` in an iterator,
   *  including trailing line end characters.  Always returns a
   *  single string with no endline, for `NumericString`.
   */
  def linesWithSeparators: Iterator[String] =
    value.linesWithSeparators

  /** Builds a new string by applying a function to all elements
   * of this `NumericString`.
   *
   *  @param f      the function to apply to each element.
   *  @return       a new string resulting from applying the given
   *                function `f` to each character of this
   *                `NumericString` and collecting the results.
   */
  def map(f: (Char) => Char): String =
    value.map(f)

  /** Finds the largest element.
   *
   *  @return   the largest character of this `NumericString`.
   */
  def max: Char =
    value.max

  /** Finds the first element which yields the largest value
   * measured by function f.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The result type of the function f.
   *  @param    f     The measuring function.
   *  @return         the first element of this `NumericString` with the
   *                  largest value measured by function f with respect to the
   *                  ordering `cmp`.
   */
  def maxBy[B](f: (Char) => B)(implicit cmp: Ordering[B]): Char =
    value.maxBy(f)

  /** Finds the smallest element.
   *
   *  @return   the smallest element of this `NumericString`.
   */
  def min: Char =
    value.min

  /** Finds the first element which yields the smallest value
   * measured by function f.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The result type of the function f.
   *  @param    f     The measuring function.
   *  @return         the first element of this `NumericString` with the
   *                  smallest value measured by function f with respect to the
   *                  ordering `cmp`.
   */
  def minBy[B](f: (Char) => B)(implicit cmp: Ordering[B]): Char =
    value.minBy(f)

  /** Displays all elements of this `NumericString` in a string.
   *
   *  @return a string representation of this
   *          `NumericString`. In the resulting string the
   *          string representations (w.r.t. the method
   *          `toString`) of all elements of this
   *          `NumericString` follow each other without any
   *          separator string.
   */
  def mkString: String =
    value.mkString

  /** Displays all elements of this `NumericString` in a string
   * using a separator string.
   *
   *  @param sep   the separator string.
   *  @return      a string representation of this
   *               `NumericString`. In the resulting string the
   *               string representations (w.r.t. the method
   *               `toString`) of all elements of this
   *               `NumericString` are separated by the string
   *               `sep`.
   *
   *  @example  `NumericString("123").mkString("|") = "1|2|3"`
   */
  def mkString(sep: String): String =
    value.mkString(sep)

  /** Displays all elements of this `NumericString` in a string
   *  using start, end, and separator strings.
   *
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      a string representation of this
   *               `NumericString`. The resulting string begins
   *               with the string `start` and ends with the
   *               string `end`. Inside, the string
   *               representations (w.r.t. the method
   *               `toString`) of all elements of this
   *               `NumericString` are separated by the string
   *               `sep`.
   *
   *  @example  `NumericString("123").mkString("(", "; ", ")") = "(1; 2; 3)"`
   */
  def mkString(start: String, sep: String, end: String): String =
    value.mkString(start, sep, end)

  /** Tests whether the `NumericString` is not empty.
   *
   *  @return `true` if the `NumericString` contains at least
   *  one element, `false` otherwise.
   */
  def nonEmpty: Boolean =
    value.nonEmpty

  /** A string copy of this `NumericString` with an element value
   * appended until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @return        a new string consisting of all elements of this
   *                 `NumericString` followed by the minimal number
   *                 of occurrences of `elem` so that the resulting
   *                 string has a length of at least `len`.
   */
  def padTo(len: Int, elem: Char): String =
    value.padTo(len, elem)

  // SKIP-SCALATESTJS,NATIVE-START
  /** Returns a parallel implementation of this collection.
   *
   *  @return  a parallel implementation of this collection
   */
  /*def par: ParSeq[Char] =
    value.par*/
  // SKIP-SCALATESTJS,NATIVE-END

  /** Partitions this `NumericString` in two strings according to a predicate.
   *
   *  @param pred the predicate on which to partition.
   *  @return     a pair of strings -- the first string consists of
   *              all elements that satisfy the predicate `p`
   *              and the second string consists of all elements
   *              that don't. The relative order of the elements
   *              in the resulting strings may not be preserved.
   */
  def partition(p: (Char) => Boolean): (String, String) =
    value.partition(p)

  /** Produces a new string where a slice of elements in this
   * `NumericString` is replaced by another sequence.
   *
   *  @param  from     the index of the first replaced element
   *  @param  patch    the replacement sequence
   *  @param  replaced the number of elements to drop in the
   *                   original `NumericString`
   *  @return          a new string consisting of all elements of this
   *                   `NumericString` except that `replaced`
   *                   elements starting from `from` are
   *                   replaced by `patch`.
   */
  def patch(from: Int, that: GenSeq[Char], replaced: Int): String =
    value.patch(from, that, replaced)

  /** Iterates over distinct permutations.
   *
   *  @return  An Iterator which traverses the distinct
   *           permutations of this `NumericString`.
   *  @example  `NumericString("122").permutations = Iterator(122, 212, 221)`
   */
  def permutations: Iterator[String] =
    value.permutations

  /** Returns the length of the longest prefix whose elements
   * all satisfy some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this `NumericString`
   *           such that every element of the segment satisfies the predicate
   *           `p`.
   */
  def prefixLength(p: (Char) => Boolean): Int =
    value.prefixLength(p)

  /** Multiplies up the elements of this collection.
   *
   *   @param num an implicit parameter defining a set of
   *                 numeric operations which includes the `*`
   *                 operator to be used in forming the product.
   *   @tparam  B    the result type of the `*` operator.
   *   @return       the product of all elements of this `NumericString`
   *                 with respect to the `*` operator in `num`.
   */
  def product[B >: Char](implicit num: Numeric[B]): B =
    value.product

  /** You can follow a `NumericString` with `.r(g1, ... , gn)`, turning
   *  it into a `Regex`, with group names g1 through gn.
   *
   *  This is not particularly useful for a `NumericString`, given the
   *  limitations on its content.
   *
   *  @param groupNames The names of the groups in the pattern,
   *  in the order they appear.
   */
  def r(groupNames: String*): Regex =
    value.r(groupNames:_*)

  /** You can follow a `NumericString` with `.r`, turning it
   * into a `Regex`.
   *
   *  This is not particularly useful for a `NumericString`, given the
   *  limitations on its content.
   */
  def r: Regex =
    value.r

  /** Reduces the elements of this `NumericString` using the
   * specified associative binary operator.
   *
   *  @tparam A1      A type parameter for the binary operator, a
   *                  supertype of `A`.
   *  @param op       A binary operator that must be associative.
   *  @return         The result of applying reduce operator `op`
   *                  between all the elements if the `NumericString` is
   *                  nonempty.
   *  @throws UnsupportedOperationException
   *                  if this `NumericString` is empty.
   */
  def reduce[A1 >: Char](op: (A1, A1) => A1): A1 =
    value.reduce(op)

  /** Applies a binary operator to all elements of this `NumericString`,
   *  going left to right.
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return       the result of inserting `op` between consecutive
   *                elements of this `NumericString`,
   *           going left to right:
   *           {{{
   *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of
   *           this `NumericString`.
   *  @throws UnsupportedOperationException if this `NumericString` is empty.
   */
  def reduceLeft[B >: Char](op: (B, Char) => B): B =
    value.reduceLeft(op)

  /** Optionally applies a binary operator to all elements of
   * this `NumericString`, going left to right.
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return       an option value containing the result of
   *                `reduceLeft(op)` if this `NumericString` is
   *                nonempty, `None` otherwise.
   */
  def reduceLeftOption[B >: Char](op: (B, Char) => B): Option[B] =
    value.reduceLeftOption(op)

  /** Reduces the elements of this `NumericString`, if any, using the specified
   *  associative binary operator.
   *
   *  @tparam A1     A type parameter for the binary operator, a
   *                 supertype of `A`.
   *  @param op      A binary operator that must be associative.
   *  @return        An option value containing result of applying
   *                 reduce operator `op` between all the
   *                 elements if the collection is nonempty, and
   *                 `None` otherwise.
   */
  def reduceOption[A1 >: Char](op: (A1, A1) => A1): Option[A1] =
    value.reduceOption(op)

  /** Applies a binary operator to all elements of this
   * `NumericString`, going right to left.
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return       the result of inserting `op` between consecutive
   *                elements of this `NumericString`,
   *           going right to left:
   *           {{{
   *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of
   *           this `NumericString`.
   *  @throws UnsupportedOperationException if this `NumericString` is empty.
   */
  def reduceRight[B >: Char](op: (Char, B) => B): B =
    value.reduceRight(op)

  /** Optionally applies a binary operator to all elements of
   *  this `NumericString`, going right to left.
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return       an option value containing the result of
   *                `reduceRight(op)` if this `NumericString` is
   *                nonempty, `None` otherwise.
   */
  def reduceRightOption[B >: Char](op: (Char, B) => B): Option[B] =
    value.reduceRightOption(op)

  /** Replace all literal occurrences of `literal` with the
   *  string `replacement`.  This is equivalent to
   *  [[java.lang.String#replaceAll]] except that both arguments
   *  are appropriately quoted to avoid being interpreted as
   *  metacharacters.
   *
   *  @param literal        the string which should be replaced
   *                        everywhere it occurs
   *  @param    replacement the replacement string
   *  @return               the resulting string
   */
  def replaceAllLiterally(literal: String, replacement: String): String =
    value.replaceAllLiterally(literal, replacement)

  def repr: String =
    value

  /** Returns new string with elements in reversed order.
   *
   *  @return A new string with all elements of this `NumericString` in
   *          reversed order.
   */
  def reverse: String =
    value.reverse

  /** An iterator yielding elements in reversed order.
   *
   * Note: `xs.reverseIterator` is the same as `xs.reverse.iterator` but
   * might be more efficient.
   *
   *  @return  an iterator yielding the elements of this `NumericString`
   *           in reversed order
   */
  def reverseIterator: Iterator[Char] =
    value.reverseIterator

  /**
   *  Builds a new collection by applying a function to all
   *  elements of this `NumericString` and collecting the
   *  results in reversed order.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @return       a new collection resulting from applying the given
   *                function `f` to each element of this
   *                `NumericString` and collecting the results
   *                in reversed order.
   */
  def reverseMap[B](f: (Char) => B): IndexedSeq[B] =
    value.reverseMap(f)

  /** Checks if the other iterable collection contains the same
   * elements in the same order as this `NumericString`.
   *
   *  @param that  the collection to compare with.
   *  @return      `true`, if both collections contain the same
   *               elements in the same order, `false` otherwise.
   */
  def sameElements[B >: Char](that: GenIterable[B]): Boolean =
    value.sameElements(that)

  /** Computes a prefix scan of the elements of the collection.
   *
   *  Note: The neutral element `z` may be applied more than once.
   *
   *  @param z          neutral element for the operator `op`
   *  @param op         the associative operator for the scan
   *
   *  @return a new string containing the prefix scan of the
   *          elements in this `NumericString`
   */
  def scan(z: Char)(op: (Char, Char) => Char) =
    value.scan(z)(op)

  /** Produces a collection containing cumulative results of applying the
   *  operator going left to right.
   *
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate
   *                 result and the element
   *  @return        collection with intermediate results
   */
  def scanLeft(z: String)(op: (String, Char) => String) =
    value.scanLeft(z)(op)

  /** Produces a collection containing cumulative results of
   *  applying the operator going right to left.  The head of
   *  the collection is the last cumulative result.
   *
   *  @param z       the initial value
   *  @param         op the binary operator applied to the intermediate
   *                 result and the element
   *  @return        collection with intermediate results
   */
  def scanRight(z: String)(op: (Char, String) => String) =
    value.scanRight(z)(op)


  /** Computes length of longest segment whose elements all
   * satisfy some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from  the index where the search starts.
   *  @return  the length of the longest segment of this `NumericString`
   *           starting from index `from` such that every
   *           element of the segment satisfies the predicate
   *           `p`.
   */
  def segmentLength(p: (Char) => Boolean, from: Int): Int =
    value.segmentLength(p, from)

  /** A version of this collection with all of the operations
   *  implemented sequentially (i.e., in a single-threaded
   *  manner).
   *
   *  This method returns a reference to this collection. In
   *  parallel collections, it is redefined to return a
   *  sequential implementation of this collection. In both
   *  cases, it has O(1) complexity.
   *
   *  @return a sequential view of the collection.
   */
  def seq: WrappedString =
    value.seq

  /** The size of this `NumericString`.
   *
   *  @return    the number of elements in this `NumericString`.
   */
  def size: Int =
    value.size

  /** Selects an interval of elements.  The returned collection is made up
   *  of all elements `x` which satisfy the invariant:
   *  {{{
   *    from <= indexOf(x) < until
   *  }}}
   *
   *  @param   from   the lowest index to include from this `NumericString`.
   *  @param   until  the lowest index to EXCLUDE from this `NumericString`.
   *  @return  a string containing the elements greater than or equal to
   *           index `from` extending up to (but not including) index `until`
   *           of this `NumericString`.
   */
  def slice(from: Int, until: Int): String =
    value.slice(from, until)

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups
   *  @return An iterator producing strings of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int, step: Int): Iterator[String] =
    value.sliding(size, step)

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *  "Sliding window" step is 1 by default.
   *  @see [[scala.collection.Iterator]], method `sliding`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing strings of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding(size: Int): Iterator[String] =
    value.sliding(size)

  /** Sorts this `NumericString` according to the Ordering which
   *  results from transforming an implicitly given Ordering
   *  with a transformation function.
   *  @see [[scala.math.Ordering]]
   *
   *  @tparam  B the target type of the transformation `f`, and the type where
   *           the ordering `ord` is defined.
   *  @param   f the transformation function mapping elements
   *           to some other domain `B`.
   *  @param   ord the ordering assumed on domain `B`.
   *  @return  a string consisting of the elements of this `NumericString`
   *           sorted according to the ordering where `x < y` if
   *           `ord.lt(f(x), f(y))`.
   *
   *  @example {{{
   *    NumericString("212").sortBy(_.toInt)
   *    res13: String = 122
   *  }}}
   */
  def sortBy[B](f: (Char) => B)(implicit ord: math.Ordering[B]): String =
    value.sortBy(f)

  /** Sorts this `NumericString` according to a comparison function.
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @param  lt  the comparison function which tests whether
   *              its first argument precedes its second argument in
   *              the desired ordering.
   *  @return     a string consisting of the elements of this `NumericString`
   *              sorted according to the comparison function `lt`.
   *  @example {{{
   *    NumericString("212").sortWith(_.compareTo(_) < 0)
   *    res14: String = 122
   *  }}}
   */
  def sortWith(lt: (Char, Char) => Boolean): String =
    value.sortWith(lt)

  /** Sorts this `NumericString` according to an Ordering.
   *
   *  The sort is stable. That is, elements that are equal (as determined by
   *  `lt`) appear in the same order in the sorted sequence as in the original.
   *
   *  @see [[scala.math.Ordering]]
   *
   *  @param  ord the ordering to be used to compare elements.
   *  @return     a string consisting of the elements of this `NumericString`
   *              sorted according to the ordering `ord`.
   */
  def sorted[B >: Char](implicit ord: math.Ordering[B]): String =
    value.sorted

  /** Splits this `NumericString` into a prefix/suffix pair
   * according to a predicate.
   *
   *  Note: `c span p`  is equivalent to (but possibly more efficient than)
   *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
   *  predicate `p` does not cause any side-effects.
   *
   *  @param p the test predicate
   *  @return  a pair of strings consisting of the longest prefix
   *           of this `NumericString` whose elements all
   *           satisfy `p`, and the rest of this
   *           `NumericString`.
   */
  def span(p: (Char) => Boolean): (String, String) =
    value.span(p)

  /**
   * Returns an array of Strings resulting from splitting this `NumericString` at all points where one of the `separators` characters is encountered.
   *
   * For more detail, see the documentation for the corresponding method in the Javadoc documentation for java.lang.String.
   *
   * @param separators    array of characters, any of which are valid split triggers
   * @return              array of strings, after splitting `NumericString` at all points where one of the `separators` characters is encountered
   */

  def split(separators: Array[Char]): Array[String] =
    value.split(separators)

  /** Split this `NumericString` around the separator character
   *
   * If this `NumericString` is the empty string, returns an array of strings
   * that contains a single empty string.
   *
   * If this `NumericString` is not the empty string, returns an array containing
   * the substrings terminated by the start of the string, the end of the
   * string or the separator character, excluding empty trailing substrings
   *
   * The behaviour follows, and is implemented in terms of <a href="http://docs.oracle.com/javase/7/docs/api/java/lang/String.html#split%28java.lang.String%29">String.split(re: String)</a>
   *
   *
   * @example {{{
   * scala> NumericString("1234").split('3')
   * res15: Array[String] = Array(12, 4)
   * //
   * //splitting the empty string always returns the array with a single
   * //empty string
   * scala> NumericString("").split('3')
   * res16: Array[String] = Array("")
   * //
   * //only trailing empty substrings are removed
   * scala> NumericString("1234").split('4')
   * res17: Array[String] = Array(123)
   * //
   * scala> NumericString("1234").split('1')
   * res18: Array[String] = Array("", 234)
   * //
   * scala> NumericString("12341").split('1')
   * res19: Array[String] = Array("", 234)
   * //
   * scala> NumericString("11211").split('1')
   * res20: Array[String] = Array("", "", 2)
   * //
   * //all parts are empty and trailing
   * scala> NumericString("1").split('1')
   * res21: Array[String] = Array()
   * //
   * scala> NumericString("11").split('1')
   * res22: Array[String] = Array()
   *  }}}
   *
   * @param separator the character used as a delimiter
   */
  def split(separator: Char): Array[String] =
    value.split(separator)

  /** Splits this `NumericString` into two at a given position.
   *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
   *         `(c take n, c drop n)`.
   *
   *  @param n the position at which to split.
   *  @return  a pair of strings consisting of the first `n`
   *           elements of this `NumericString`, and the other elements.
   */
  def splitAt(n: Int): (String, String) =
    value.splitAt(n)

  /** Defines the prefix of this object's `toString` representation.
   *
   *  @return a string representation which starts the result of
   *           `toString` applied to this `NumericString`. By
   *           default the string prefix is the simple name of
   *           the collection class `NumericString`.
   */
  def stringPrefix: String =
    org.scalactic.ColCompatHelper.className(value)

  /**
   *  Strip trailing line end character from this string if it has one.
   *
   *  A line end character is one of
   *  - `LF` - line feed   (`0x0A` hex)
   *  - `FF` - form feed   (`0x0C` hex)
   *
   *  If a line feed character `LF` is preceded by a carriage return `CR`
   *  (`0x0D` hex), the `CR` character is also stripped (Windows convention).
   */
  def stripLineEnd: String  =
    value.stripLineEnd

  /**
   * Strip a leading prefix consisting of blanks or control characters
   * followed by `|` from the line.
   */
  def stripMargin: String =
    value.stripMargin

  /**
   *  Strip a leading prefix consisting of blanks or control characters
   *  followed by `marginChar` from the line.
   */
  def stripMargin(marginChar: Char): String =
    value.stripMargin(marginChar)

  /** Returns this `NumericString` as a string with the given
   * `prefix` stripped.  If this `NumericString` does not start
   * with `prefix`, it is returned unchanged.
   */
  def stripPrefix(prefix: String): String =
    value.stripPrefix(prefix)

  /** Returns this `NumericString` as a string with the given
   *  `suffix` stripped. If this `NumericString` does not end
   *  with `suffix`, it is returned unchanged.
   */
  def stripSuffix(suffix: String): String =
    value.stripSuffix(suffix)

  /** Sums up the elements of this collection.
   *
   * @return the sum of all character values in this `NumericString`
   */
  def sum[B >: Char](implicit num: Numeric[B]): B =
    value.sum

  /** Selects all elements except the first.
   *
   *  @return  a string consisting of all elements of this `NumericString`
   *           except the first one.
   *  @throws UnsupportedOperationException if the `NumericString` is empty.
   */
  def tail: String =
    value.tail

  /** Iterates over the tails of this `NumericString`. The first value will be this
   *  `NumericString` as a string and the final one will be an empty string, with the intervening
   *  values the results of successive applications of `tail`.
   *
   *  @return   an iterator over all the tails of this `NumericString`
   *  @example {{{
   * scala> NumericString("123").tails.toList
   * res31: List[String] = List(123, 23, 3, "")
   * }}}
   */
  def tails: Iterator[String] =
    value.tails

  /** Selects first ''n'' elements.
   *
   *  @param  n    the number of elements to take from this `NumericString`.
   *  @return      a string consisting only of the first `n` elements
   *               of this `NumericString`, or else the whole
   *               `NumericString`, if it has less than `n` elements.
   */
  def take(n: Int): String =
    value.take(n)

  /** Selects last ''n'' elements.
   *
   *  @param n the number of elements to take
   *  @return   a string consisting only of the last `n` elements
   *            of this `NumericString`, or else the whole
   *            `NumericString`, if it has less than `n` elements.
   */
  def takeRight(n: Int): String =
    value.takeRight(n)

  /** Takes longest prefix of elements that satisfy a predicate.
   *
   *  @param   p  The predicate used to test elements.
   *  @return     the longest prefix of this `NumericString` whose
   *              elements all satisfy the predicate `p`.
   */
  def takeWhile(p: (Char) => Boolean): String =
    value.takeWhile(p)

  /** Converts this `NumericString` to an array.
   *
   *  @return    an array containing all elements of this `NumericString`.
   */
  def toArray: Array[Char] =
    value.toArray

  /** Uses the contents of this `NumericString` to create a new mutable buffer.
   *
   *  @return a buffer containing all elements of this `NumericString`.
   */
  def toBuffer[A1 >: Char]: Buffer[A1] =
    value.toBuffer[A1]

  /**
   * Parse as a `Byte`
   * @throws java.lang.NumberFormatException If the string does
   *    not contain a parsable `Byte`.
   */
  def toByte: Byte =
    value.toByte

  /**
    * Parse as a `Double`.
    * @throws java.lang.NumberFormatException If the string does
    *   not contain a parsable `Double`.
   */
  def toDouble: Double =
    value.toDouble

  /**
    * Parse as a `Float`.
    * @throws java.lang.NumberFormatException If the string does
    *    not contain a parsable `Float`.
   */
  def toFloat: Float =
    value.toFloat

  /** Converts this `NumericString` to an indexed sequence.
   *
   *  @return an indexed sequence containing all elements of
   *    this `NumericString`.
   */
  def toIndexedSeq: IndexedSeq[Char] =
    value.toIndexedSeq

  /**
   * Parse as an `Int`
   * @throws java.lang.NumberFormatException If the string does
   *   not contain a parsable `Int`.
   */
  def toInt: Int =
    value.toInt

  /** Converts this `NumericString` to an iterable collection.
   *
   *  @return an `Iterable` containing all elements of this `NumericString`.
   */
  def toIterable: collection.Iterable[Char] =
    value.toIterable

  /** Returns an Iterator over the elements in this `NumericString`.
   *
   *  @return an Iterator containing all elements of this `NumericString`.
   */
  def toIterator: Iterator[Char] =
    value.toIterator

  /** Converts this `NumericString` to a list.
   *
   *  @return a list containing all elements of this `NumericString`.
   */
  def toList: scala.List[Char] =
    value.toList

  /**
   * Parse as a `Long`.
   * @throws java.lang.NumberFormatException If the string does
   *   not contain a parsable `Long`.
   */
  def toLong: Long =
    value.toLong

  /** Converts this `NumericString` to a sequence.
   *
   *  @return a sequence containing all elements of this `NumericString`.
   */
  def toSeq: collection.Seq[Char] =
    value.toSeq

  /** Converts this `NumericString` to a set.
   *
   *  @return      a set containing all elements of this `NumericString`.
   */
  def toSet[B >: Char]: Set[B] =
    value.toSet

  /**
   * Parse as a `Short`.
   * @throws java.lang.NumberFormatException If the string does
   *   not contain a parsable `Short`.
   */
  def toShort: Short =
    value.toShort

  /** Converts this `NumericString` to a stream.
   *  @return a stream containing all elements of this `NumericString`.
   */
  def toStream: Stream[Char] =
    value.toStream

  /** Converts this `NumericString` to an unspecified Traversable.
   *
   *  @return a Traversable containing all elements of this `NumericString`.
   */
  def toTraversable: collection.Traversable[Char] =
    value.toTraversable

  /** Converts this `NumericString` to a Vector.
   *
   *  @return a vector containing all elements of this `NumericString`.
   */
  def toVector: scala.Vector[Char] =
    value.toVector

  /** Produces a new sequence which contains all elements of
   *  this `NumericString` and also all elements of a given
   *  sequence. `xs union ys` is equivalent to `xs ++ ys`.
   *
   *    Another way to express this is that `xs union ys`
   *    computes the order-preserving multi-set union of `xs`
   *    and `ys`.  `union` is hence a counter-part of `diff` and
   *    `intersect` which also work on multi-sets.
   *
   *    @return a new string which contains all elements of this
   *                  `NumericString` followed by all elements
   *                  of `that`.
   */
  def union(that: Seq[Char]) =
    value.union(that)

  /** A copy of this `NumericString` with one single replaced element.
   *
   *  @param  index  the position of the replacement
   *  @param  elem   the replacing element
   *  @return a string copy of this `NumericString` with the
   *    element at position `index` replaced by `elem`.
   *  @throws IndexOutOfBoundsException if `index` does not
   *    satisfy `0 <= index < length`.
   */
  def updated(index: Int, elem: NumericChar): NumericString =
    NumericString.ensuringValid(value.updated(index, elem.value))

  /** Creates a non-strict view of this `NumericString`.
   *
   *  @return a non-strict view of this `NumericString`.
   */
  def view =
    value.view

  /** Creates a non-strict filter of this `NumericString`.
   *
   *  Note: the difference between `c filter p` and `c withFilter p` is that
   *        the former creates a new collection, whereas the latter only
   *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
   *        and `withFilter` operations.
   *
   *  @param p   the predicate used to test elements.
   *  @return    an object of class `WithFilter`, which supports
   *             `map`, `flatMap`, `foreach`, and `withFilter`
   *             operations.  All these operations apply to
   *             those elements of this `NumericString` which
   *             satisfy the predicate `p`.
   */
  def withFilter(p: (Char) => Boolean) =
    value.withFilter(p)

  /** Returns a <code>Iterable</code> of pairs formed from this `NumericString`
   *  and another iterable collection by combining corresponding
   *  elements in pairs.  If one of the two collections is
   *  longer than the other, its remaining elements are ignored.
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a collection containing pairs consisting of
   *                 corresponding elements of this
   *                 `NumericString` and `that`. The length of
   *                 the returned collection is the minimum of
   *                 the lengths of this `NumericString` and
   *                 `that`.
   */
  def zip[B](that: Iterable[B]): Iterable[(Char, B)] =
    value.zip(that)

  /** Returns a collection of pairs formed from this
   *  `NumericString` and another iterable collection by
   *  combining corresponding elements in pairs.  If one of the
   *  two collections is shorter than the other, placeholder
   *  elements are used to extend the shorter collection to the
   *  length of the longer.
   *
   *  @tparam  B      the type of the second half of the returned pairs
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the
   *                  result if this `NumericString` is shorter than `that`.
   *  @param thatElem the element to be used to fill up the
   *                  result if `that` is shorter than this `NumericString`.
   *  @return   a new <code>Iterable</code> consisting of corresponding
   *            elements of this `NumericString` and
   *            `that`. The length of the returned
   *            collection is the maximum of the lengths
   *            of this `NumericString` and `that`.  If
   *            this `NumericString` is shorter than
   *            `that`, `thisElem` values are used to pad
   *            the result.  If `that` is shorter than
   *            this `NumericString`, `thatElem` values
   *            are used to pad the result.
   */
  def zipAll[A1 >: Char, B](that: Iterable[B], thisElem: A1, thatElem: B): Iterable[(A1, B)] =
    value.zipAll(that, thisElem, thatElem)

  /** Zips this `NumericString` with its indices.
   *
   *    @return A new <code>Iterable</code> containing pairs
   *                   consisting of all characters of this
   *                   `NumericString` paired with their
   *                   index. Indices start at `0`.
   *    @example {{{
   * scala> NumericString("123").zipWithIndex
   * res41: scala.collection.immutable.IndexedSeq[(Char, Int)] = Vector((1,0), (2,1), (3,2))
   * }}}
   */
  def zipWithIndex: Iterable[(Char, Int)] =
    value.zipWithIndex

  /**
   * Applies the passed <code>String =&gt; String</code> function to the
   * underlying <code>String</code> value, and if the result is a numeric
   * string, returns the result wrapped in a <code>NumericString</code>,
   * else throws <code>AssertionError</code>.
   *
   * A factory/assertion method that produces a <code>NumericString</code>
   * given a valid <code>String</code> value, or throws
   * <code>AssertionError</code>, if given an invalid <code>String</code> value.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you
   * think it will always succeed to document your reasoning. If you are not
   * sure an `ensuringValid` call will always succeed, you should use one of
   * the other factory or validation methods provided on this object instead:
   * `isValid`, `tryingValid`, `passOrElse`, `goodOrElse`, or `rightOrElse`.
   *
   * <p>
   * This method will inspect the result of applying the given function to this
   * <code>NumericString</code>'s underlying <code>String</code> value and if
   * the result is a valid numeric string, it will return a
   * <code>NumericString</code> representing that value. Otherwise, the
   * <code>String</code> value returned by the given function is
   * not a valid numeric string, so this method will throw
   * <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This method differs from a vanilla <code>assert</code> or
   * <code>ensuring</code> call in that you get something you didn't already
   * have if the assertion succeeds: a <em>type</em> that promises a
   * <code>String</code> contains only numeric digit characters. With this
   * method, you are asserting that you are convinced the result of
   * the computation represented by applying the given function to this
   * <code>NumericString</code>'s value will produce a valid numeric string.
   * Instead of producing an invalid <code>NumericString</code>, this method
   * will signal an invalid result with a loud <code>AssertionError</code>.
   * </p>
   *
   * @param f the <code>String =&gt; String</code> function to apply to this
   *     <code>NumericString</code>'s underlying <code>String</code> value.
   * @return the result of applying this <code>NumericString</code>'s
   *     underlying <code>String</code> value to to the passed function,
   *     wrapped in a <code>NumericString</code> if it is a valid numeric
   *     string (else throws <code>AssertionError</code>).
   * @throws AssertionError if the result of applying this
   *     <code>NumericString</code>'s underlying <code>String</code> value to
   *     to the passed function contains non-digit characters.
   */
  def ensuringValid(f: String => String): NumericString = {
    val candidateResult: String = f(value)
    if (NumericStringMacro.isValid(candidateResult)) new NumericString(candidateResult)
    else throw new AssertionError(s"$candidateResult, the result of applying the passed function to $value, was not a valid NumericString")
  }
}

/**
 * The companion object for <code>NumericString</code> that offers factory
 * methods that produce <code>NumericString</code>s.
 */
object NumericString {

  /**
   * A factory method that produces an <code>Option[NumericString]</code>
   * given a <code>String</code> value.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a numeric <code>String</code>, <em>i.e.</em>, one that doesn't
   * contain any non-digit characters, it will return a
   * <code>NumericString</code> representing that value, wrapped in a
   * <code>Some</code>. Otherwise, the passed <code>String</code>
   * value is not a numeric string value, so this method will return
   * <code>None</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas <code>from</code>
   * inspects <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     wrapped in a <code>Some[NumericString]</code>.
   * @return the specified <code>String</code> value wrapped
   *     in a <code>Some[NumericString]</code>, if it is numeric, else
   *     <code>None</code>.
   */
  def from(value: String): Option[NumericString] =
    if (NumericStringMacro.isValid(value)) Some(new NumericString(value)) else None

  /**
   * A factory/assertion method that produces a <code>NumericString</code>
   * given a valid <code>String</code> value, or throws
   * <code>AssertionError</code>, if given an invalid <code>String</code> value.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you
   * think it will always succeed to document your reasoning. If you are not
   * sure an `ensuringValid` call will always succeed, you should use one of
   * the other factory or validation methods provided on this object instead:
   * `isValid`, `tryingValid`, `passOrElse`, `goodOrElse`, or `rightOrElse`.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a valid numeric string, it will return a <code>NumericString</code>
   * representing that value.  Otherwise, the passed <code>String</code>
   * value is not a valid numeric string, so this method will throw
   * <code>AssertionError</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas this method inspects
   * <code>String</code> values at run time.
   * It differs from a vanilla <code>assert</code> or <code>ensuring</code>
   * call in that you get something you didn't already have if the assertion
   * succeeds: a <em>type</em> that promises a <code>String</code> is numeric.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     wrapped in a <code>NumericString</code>.
   * @return the specified <code>String</code> value wrapped in a
   *     <code>NumericString</code>, if it is numeric, else throws
   *     <code>AssertionError</code>.
   * @throws AssertionError if the passed value is not numeric
   */
  def ensuringValid(value: String): NumericString =
    if (NumericStringMacro.isValid(value)) new NumericString(value) else {
      throw new AssertionError(s"$value was not a valid NumericString")
    }

  /**
   * A predicate method that returns true if a given
   * <code>String</code> value contains only numeric digit characters (0-9).
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     true.
   * @return true if the specified <code>String</code> is numeric, else false.
   */
  def isValid(value: String): Boolean = NumericStringMacro.isValid(value)

  /**
   * A factory method that produces a <code>NumericString</code> given a
   * <code>String</code> value and a default <code>NumericString</code>.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a valid numeric string, <em>i.e.</em>, a <code>String</code>
   * containing only numeric digit characters (0-9), it will return a
   * <code>NumericString</code> representing that value.
   * Otherwise, the passed <code>String</code> value contains non-digit
   * characters, so this method will return the passed <code>default</code>
   * value.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code>
   * factory method in that <code>apply</code> is implemented
   * via a macro that inspects <code>String</code> literals at
   * compile time, whereas <code>fromOrElse</code> inspects
   * <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return.
   * @param default the <code>NumericString</code> to return if the passed
   *     <code>String</code> value is not numeric.
   * @return the specified <code>String</code> value wrapped in a
   *     <code>NumericString</code>, if it is numeric, else the
   *     <code>default</code> <code>NumericString</code> value.
   */
  def fromOrElse(value: String, default: => NumericString): NumericString =
    if (NumericStringMacro.isValid(value)) new NumericString(value) else default

  /**
   * A factory method, implemented via a macro, that produces a
   * <code>NumericString</code> if passed a valid <code>String</code> literal,
   * otherwise a compile time error.
   *
   * <p>
   * The macro that implements this method will inspect the specified
   * <code>String</code> expression at compile time. If the expression is a
   * numeric <code>String</code> literal, <em>i.e.</em>, it doesn't contain
   * any non-digit characters (0-9), it will return a
   * <code>NumericString</code> representing that value. Otherwise, the passed
   * <code>String</code> expression is either a literal that contains non-digit
   * characters, or is not a literal, so this method will give a compiler error.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>from</code> factory method
   * in that this method is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas <code>from</code>
   * inspects <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> literal expression to inspect at
   *     compile time, and if it is a numeric string, to return wrapped in a
   *     <code>NumericString</code> at run time.
   * @return the specified, valid <code>String</code> literal value wrapped
   *     in a <code>NumericString</code>. (If the specified expression is not
   *     a valid <code>String</code> literal, the invocation of this method
   *     will not compile.)
   */
  inline def apply(inline value: String): NumericString = ${ NumericStringMacro('{value}) }

  /**
   * A factory/validation method that produces a <code>NumericString</code>,
   * wrapped in a <code>Success</code>, given a valid <code>String</code>
   * value, or if the given <code>String</code> is invalid, an
   * <code>AssertionError</code>, wrapped in a <code>Failure</code>.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it represents a numeric value, it will return a <code>NumericString</code>
   * representing that value, wrapped in a <code>Success</code>.
   * Otherwise, the passed <code>String</code> value is not numeric, so this
   * method will return an <code>AssertionError</code>, wrapped in a
   * <code>Failure</code>.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas this method inspects
   * <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     wrapped in a <code>Success(NumericString)</code>.
   * @return the specified <code>String</code> value wrapped
   *     in a <code>Success(NumericString)</code>, if it is numeric, else a
   *     <code>Failure(AssertionError)</code>.
   */
   def tryingValid(value: String): Try[NumericString] =
     if (NumericStringMacro.isValid(value))
       Success(new NumericString(value))
     else
       Failure(new AssertionError(value + " was not a valid NumericString"))

  /**
   * A validation method that produces a <code>Pass</code>
   * given a valid <code>String</code> value, or
   * an error value of type <code>E</code> produced by passing the
   * given <em>invalid</em> <code>String</code> value
   * to the given function <code>f</code>, wrapped in a <code>Fail</code>.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a numeric <code>String</code>, it will return a <code>Pass</code>.
   * Otherwise, the passed <code>String</code> value is non-numeric, so this
   * method will return a result of type <code>E</code> obtained by passing
   * the invalid <code>String</code> value to the given function <code>f</code>,
   * wrapped in a `Fail`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas this method inspects
   * <code>String</code> values at run time.
   * </p>
   *
   * @param value the `String` to validate that it is numeric.
   * @return a `Pass` if the specified `String` value is numeric,
   *   else a `Fail` containing an error value produced by passing the
   *   specified `String` to the given function `f`.
   */
  def passOrElse[E](value: String)(f: String => E): Validation[E] =
    if (NumericStringMacro.isValid(value)) Pass else Fail(f(value))

  /**
   * A factory/validation method that produces a <code>NumericString</code>,
   * wrapped in a <code>Good</code>, given a valid <code>String</code> value,
   * or if the given <code>String</code> is invalid, an error value of type
   * <code>B</code> produced by passing the given <em>invalid</em>
   * <code>String</code> value to the given function <code>f</code>, wrapped
   * in a <code>Bad</code>.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a numeric <code>String</code>, it will return a
   * <code>NumericString</code> representing that value, wrapped in a
   * <code>Good</code>. Otherwise, the passed <code>String</code> value is
   * NOT numeric, so this method will return a result of type <code>B</code>
   * obtained by passing the invalid <code>String</code> value to the given
   * function <code>f</code>, wrapped in a `Bad`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas this method inspects
   * <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     wrapped in a <code>Good(NumericString)</code>.
   * @return the specified <code>String</code> value wrapped
   *     in a <code>Good(NumericString)</code>, if it is numeric, else a
   *     <code>Bad(f(value))</code>.
   */
  def goodOrElse[B](value: String)(f: String => B): NumericString Or B =
    if (NumericStringMacro.isValid(value)) Good(NumericString.ensuringValid(value)) else Bad(f(value))

  /**
   * A factory/validation method that produces a <code>NumericString</code>,
   * wrapped in a <code>Right</code>, given a valid <code>String</code> value,
   * or if the given <code>String</code> is invalid, an error value of type
   * <code>L</code> produced by passing the given <em>invalid</em>
   * <code>String</code> value to the given function <code>f</code>, wrapped
   * in a <code>Left</code>.
   *
   * <p>
   * This method will inspect the passed <code>String</code> value and if
   * it is a numeric <code>String</code>, it will return a
   * <code>NumericString</code> representing that value, wrapped in a
   * <code>Right</code>. Otherwise, the passed <code>String</code> value is
   * NOT numeric, so this method will return a result of type <code>L</code>
   * obtained by passing the invalid <code>String</code> value to the given
   * function <code>f</code>, wrapped in a `Left`.
   * </p>
   *
   * <p>
   * This factory method differs from the <code>apply</code> factory method
   * in that <code>apply</code> is implemented via a macro that inspects
   * <code>String</code> literals at compile time, whereas this method inspects
   * <code>String</code> values at run time.
   * </p>
   *
   * @param value the <code>String</code> to inspect, and if numeric, return
   *     wrapped in a <code>Right(NumericString)</code>.
   * @return the specified <code>String</code> value wrapped
   *     in a <code>Right(NumericString)</code>, if it is numeric, else a
   *     <code>Left(f(value))</code>.
   */
  def rightOrElse[L](value: String)(f: String => L): Either[L, NumericString] =
    if (NumericStringMacro.isValid(value)) Right(NumericString.ensuringValid(value)) else Left(f(value))
}

