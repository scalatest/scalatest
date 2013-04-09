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
package org.scalautils

/**
 * Class used via an implicit conversion to enable any two objects to be compared with
 * <code>===</code> and <code>!==</code> with an <code>Option[String]</code> result and no enforced type constraint between
 * two object types. For example:
 *
 * <pre class="stHighlight">
 * assert(a === b)
 * assert(c !== d)
 * </pre>
 *
 * <p>
 * You can also check numeric values against another with a tolerance. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(a === (2.0 +- 0.1))
 * assert(c !== (2.0 +- 0.1))
 * </pre>
 *
 * <p>
 * The benefit of using <code>assert(a === b)</code> rather than <code>assert(a == b)</code> in ScalaTest code is
 * that a <code>TestFailedException</code> produced by the former will include the values of <code>a</code> and <code>b</code>
 * in its detail message.
 * </p>
 *
 * <p>
 * <em>
 * Note: This class has "Legacy" in its name because its approach to error messages will eventually be replaced by macros. Once ScalaTest no longer supports Scala 2.9, 
 * this class will be deprecated in favor of class <code>Equalizer</code>. Instead of obtaining nice error messages via the <code>Option[String]</code>
 * returned by the methods of this class, the error messages will be obtained by a macro. The "legacy" approach to good error messages will continue to be
 * used, however, until ScalaTest no longer supports Scala 2.9, since macros were introduced to Scala (in experimental form) in 2.10.
 * </em>
 * </p>
 *
 * <p>
 * The primary constructor takes one object, <code>left</code>, whose type is being converted to <code>Equalizer</code>. The <code>left</code>
 * value may be a <code>null</code> reference, because this is allowed by Scala's <code>==</code> operator.
 * </p>
 *
 * @param left An object to convert to <code>Equalizer</code>, which represents the <code>left</code> value
 *     of a <code>===</code> or <code>!==</code> equality check.
 *
 * @author Bill Venners
 */
class LegacyEqualizer[L](left: L) {

  private def diffStrings(s: String, t: String): Tuple2[String, String] = {
    def findCommonPrefixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the prefix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(i) != t.charAt(i))
        if (!found)
          i = i + 1
      }
      i
    }
    def findCommonSuffixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the suffix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
        if (!found)
          i = i + 1
      }
      i
    }
    val commonPrefixLength = findCommonPrefixLength(s, t)
    val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
    val prefix = s.substring(0, commonPrefixLength)
    val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
    val sMiddleEnd = s.length - commonSuffixLength
    val tMiddleEnd = t.length - commonSuffixLength
    val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
    val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
    val MaxContext = 20
    val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
    val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
    (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
  }

  // If the objects are two strings, replace them with whatever is returned by diffStrings.
  // Otherwise, use the same objects.
  private def getObjectsForFailureMessage(a: Any, b: Any) =
    a match {
      case aStr: String => {
        b match {
          case bStr: String => {
            diffStrings(aStr, bStr)
          }
          case _ => (a, b)
        }
      }
      case _ => (a, b)
    }

  /**
   * Compare two objects for equality, returning an <code>Option[String]</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for equality with <code>left</code>, passed to the constructor
   * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
   * @return None if the <code>left</code> and <code>right</code> objects are equal according to the passed <code>Equality</code> type class.
   *    else returns an error message string wrapped in a <code>Some</code>.
   */
  def ===(right: Any)(implicit equality: Equality[L]): Option[String] = 
    if (equality.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("didNotEqual", leftee, rightee))
    }

  /**
   * Compare two objects for inequality, returning an <code>Option[String]</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
   *
   * @param right the object to compare for inequality with <code>left</code>, passed to the constructor
   * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
   * @return None if the <code>left</code> and <code>right</code> objects are <em>not</em> equal according to the passed <code>Equality</code> type class.
   *    else returns an error message string wrapped in a <code>Some</code>.
   */
  def !==(right: Any)(implicit equality: Equality[L]): Option[String] =
    if (!equality.areEqual(left, right))
      None
    else {
      val (leftee, rightee) = getObjectsForFailureMessage(left, right)
      Some(FailureMessages("equaled", leftee, rightee))
    }

  /**
   * Determine whether a numeric object is within the passed <code>Interval</code>, returning an <code>Option[String]</code>.
   *
   * @param interval the <code>Interval</code> against which to compare the value passed to the constructor as <code>left</code> 
   * @return None if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Interval</code> passed to this method, 
   *    else returns an error message string wrapped in a <code>Some</code>.
   */
  def ===(interval: Interval[L]): Option[String] =
    if (interval == null) {
      if (left == null)
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, interval)
        Some(FailureMessages("equaled", leftee, rightee))
      }
    }
    else {
      if (interval.isWithin(left))
        None
      else
        Some(FailureMessages("wasNotPlusOrMinus", left, interval.pivot, interval.tolerance))
    }

  /**
   * Determine whether a numeric object is outside the passed <code>Interval</code>, returning an <code>Option[String]</code>.
   *
   * @param interval the <code>Interval</code> against which to compare the value passed to the constructor as <code>left</code> 
   * @return true if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Interval</code> passed to this method.
   *    else returns an error message string wrapped in a <code>Some</code>.
   */
  def !==(interval: Interval[L]): Option[String] =
    if (interval == null) {
      if (left != null)
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, interval)
        Some(FailureMessages("equaled", leftee, rightee))
      }
    }
    else {
      if (if (interval != null) !interval.isWithin(left) else left != interval)
        None
      else
        Some(FailureMessages("wasPlusOrMinus", left, interval.pivot, interval.tolerance))
    }
}

