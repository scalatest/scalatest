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
package org.scalactic

import TripleEqualsSupport._

/**
 * Trait that defines abstract methods used to enforce compile-time type constraints for equality comparisons, and defines <code>===</code> and <code>!==</code> operators
 * used by matchers.
 *
 * <p>
 * The abstract methods of this trait are selectively implemented as implicit by subclasses to enable a spectrum of type constraints for the
 * <code>===</code> and <code>!==</code> operators. As an illustration, if in the expression, <code>a === b</code>, the type of <code>a</code>
 * is <code>A</code> and <code>b</code> is <code>B</code>, the following three levels of compile-time checking can be obtained from
 * <code>TripleEqualsSupport</code> subtraits:
 * </p>
 *
 * <p>
 * <b>Unchecked</b> - <code>A</code> and <code>B</code> can be any two types. This (weakest) constraint level is available from
 * subtraits <a href="TripleEquals.html"><code>TripleEquals</code></a> and <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>.
 * </p>
 * 
 * <p>
 * <b>Conversion checked</b> - <code>A</code> must be a subtype of <code>B</code>, or vice versa, or an implicit conversion must be available that converts
 * <code>A</code> to <code>B</code>, or vice versa. (Both <code>A</code> and <code>B</code> can be the same type, because a type is considered a subtype
 * of itself.)
 * This (intermediate) constraint level is available from subtraits <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> and <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>.
 * </p>
 * 
 * <p>
 * <b>Type checked</b> - <code>A</code> must be a subtype of <code>B</code>, or vice versa.
 * (Both <code>A</code> and <code>B</code> can be the same type, because a type is considered a subtype
 * of itself.)
 * This (strongest) constraint level is available from subtraits <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>.
 * </p>
 *
 * <p>
 * The difference between the regular and &ldquo;legacy&rdquo; variants of each pair of traits is that the  <code>===</code> and <code>!==</code> operators
 * provided by the regular variants result in <code>Boolean</code>, whereas those of the legacy variants result in <code>Option[String]</code>. For example, were you
 * to mix in <code>TripleEquals</code>, the expression <code>1 + 1 === 3</code> would return <code>false</code>. Were you to mix in <code>LegacyTripleEquals</code>,
 * by contrast, the expression <code>1 + 1 === 3</code> would return <code>Some("2 did not equal 3")</code>.
 * </p>
 *
 * <p>
 * The purpose of the legacy variants is to maintain compatibility with
 * existing code that uses ScalaTest's original <code>===</code> defined in trait <a href="../scalatest/Assertions.html"><code>org.scalatest.Assertions</code></a>. This
 * <code>===</code> operator returned an
 * <code>Option[String]</code> to facilitate better error messages. With the advent of macros in Scala 2.10, it is possible to obtain good error messages by making
 * <code>assert</code> a macro. Once ScalaTest no longer supports Scala 2.9, the legacy variants (<code>LegacyTripleEquals</code>,
 * <code>ConversionCheckedLegacyTripleEquals</code>, and <code>TypeCheckedLegacyTripleEquals</code>) will be deprecated and eventually removed, <code>===</code> will
 * return only <code>Boolean</code>, and good error
 * messages will be obtained via macros. 
 * </p>
 * 
 * <p>
 * This trait defines all methods that need to be defined implicitly by the six subtraits so that if multiple subtraits are used together, the inner-most
 * subtrait in scope can not only enable the implicits it needs by overriding or hiding those methods (currently-in-scope as regular, non-implicit methods) and making
 * them implicit, it can also <em>disable</em> any implicits enabled by its sibling subtraits in enclosing scopes. For example, if your test class mixes
 * in <code>TypeCheckedTripleEquals</code>, inside your test class the following methods will be implicit:
 * </p>
 *
 * <ul>
 * <li><code>convertToCheckingEqualizer</code></li>
 * <li><code>typeCheckedConstraint</code></li>
 * <li><code>lowPriorityTypeCheckedConstraint</code></li>
 * <li><code>convertEquivalenceToAToBConstraint</code></li>
 * <li><code>convertEquivalenceToBToAConstraint</code></li>
 * </ul>
 * 
 * <p>
 * If in the body of a test you want to turn off the type checking, you can import the members
 * of <code>TripleEquals</code> in the body of that test. This will not only hide
 * non-implicit methods <code>convertToEqualizer</code> <code>unconstrainedEquality</code> of <code>TypeCheckedTripleEquals</code>,
 * replacing those with implicit ones defined in <code>TripleEquals</code>, it will also hide the three methods made implicit in <code>TypeCheckedTripleEquals</code>
 * (and listed above), replacing them by <em>non-implicit</em> ones.
 * </p>
 * 
 * <p>
 * In short, you should be able to select a primary constraint level via either a mixin or import, then change that in nested scopes
 * however you want, again either through a mixin or import, without getting any implicit conversion ambiguity. The innermost constraint level in scope
 * will always be in force.
 * <p>
 * 
 * @author Bill Venners
 */
trait TripleEqualsSupport {

  /**
   * Class used via an implicit conversion to enable any two objects to be compared with
   * <code>===</code> and <code>!==</code> with a <code>Boolean</code> result and no enforced type constraint between
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
   * @param leftSide An object to convert to <code>Equalizer</code>, which represents the value
   *     on the left side of a <code>===</code> or <code>!==</code> invocation.
   *
   * @author Bill Venners
   */
  class Equalizer[L](val leftSide: L) { // Note: This is called leftSide not left to avoid a conflict with scalaz's implicit that adds left
  
    /**
     * Compare two objects for equality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
     *
     * @param rightSide the object to compare for equality with <code>leftSide</code>, passed to the constructor
     * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
     * @return true if the <code>leftSide</code> and <code>rightSide</code> objects are equal according to the passed <code>Equality</code> type class.
     */
    def ===(rightSide: Any)(implicit equality: Equality[L]): Boolean = equality.areEqual(leftSide, rightSide)
  
    /**
     * Compare two objects for inequality, returning a <code>Boolean</code>, using the <code>Equality</code> type class passed as <code>equality</code>.
     *
     * @param rightSide the object to compare for inequality with <code>leftSide</code>, passed to the constructor
     * @param equality an implicit <code>Equality</code> type class that defines a way of calculating equality for objects of type <code>L</code>
     * @return true if the <code>leftSide</code> and <code>rightSide</code> objects are <em>not</em> equal according to the passed <code>Equality</code> type class.
     */
    def !==(rightSide: Any)(implicit equality: Equality[L]): Boolean = !equality.areEqual(leftSide, rightSide)
  
    /**
     * Determine whether a numeric object is within the passed <code>Spread</code>, returning a <code>Boolean</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>leftSide</code> 
     * @return true if the value passed to the constructor as <code>leftSide</code> is within the <code>Spread</code> passed to this method.
     */
    def ===(spread: Spread[L]): Boolean = if (spread != null) spread.isWithin(leftSide) else leftSide == spread
  
    /**
     * Determine whether a numeric object is outside the passed <code>Spread</code>, returning a <code>Boolean</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>leftSide</code> 
     * @return true if the value passed to the constructor as <code>leftSide</code> is <em>not</em> within the <code>Spread</code> passed to this method.
     */
    def !==(spread: Spread[L]): Boolean = if (spread != null) !spread.isWithin(leftSide) else leftSide != spread
  
    /**
     * Determine whether an object reference is <code>null</code>.
     *
     * @param literalNull a <code>null</code> value against which to compare the value passed to the constructor as <code>leftSide</code> for equality
     * @return true if the value passed to the constructor as <code>leftSide</code> is <code>null</code>.
     */
    def ===(literalNull: Null): Boolean = leftSide == null
  
    /**
     * Determines whether an object reference is non-<code>null</code>.
     *
     * @param literalNull a <code>null</code> value against which to compare the value passed to the constructor as <code>leftSide</code> for inequality
     * @return true if the value passed to the constructor as <code>leftSide</code> is non-<code>null</code>.
     */
    def !==(literalNull: Null): Boolean = leftSide != null
  }

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
     * Determine whether a numeric object is within the passed <code>Spread</code>, returning an <code>Option[String]</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>left</code> 
     * @return None if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Spread</code> passed to this method, 
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def ===(spread: Spread[L]): Option[String] =
      if (spread == null) {
        if (left == null)
          None
        else {
          val (leftee, rightee) = getObjectsForFailureMessage(left, spread)
          Some(FailureMessages("didNotEqual", leftee, rightee))
        }
      }
      else {
        if (spread.isWithin(left))
          None
        else
          Some(FailureMessages("wasNotPlusOrMinus", left, spread.pivot, spread.tolerance))
      }
  
    /**
     * Determine whether a numeric object is outside the passed <code>Spread</code>, returning an <code>Option[String]</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>left</code> 
     * @return true if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Spread</code> passed to this method.
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def !==(spread: Spread[L]): Option[String] =
      if (spread == null) {
        if (left != null)
          None
        else {
          val (leftee, rightee) = getObjectsForFailureMessage(left, spread)
          Some(FailureMessages("equaled", leftee, rightee))
        }
      }
      else {
        if (if (spread != null) !spread.isWithin(left) else left != spread)
          None
        else
          Some(FailureMessages("wasPlusOrMinus", left, spread.pivot, spread.tolerance))
      }
  }
  
  /**
   * Class used via an implicit conversion to enable two objects to be compared with
   * <code>===</code> and <code>!==</code> with a <code>Boolean</code> result and an enforced type constraint between
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
   * @param leftSide An object to convert to <code>Equalizer</code>, which represents the value
   *     on the left side of a <code>===</code> or <code>!==</code> invocation.
   *
   * @author Bill Venners
   */
  class CheckingEqualizer[L](val leftSide: L) { // Note: This is called leftSide not left to avoid a conflict with scalaz's implicit that adds left
  
    /**
     * Compare two objects for equality, returning a <code>Boolean</code>, using the <code>Constraint</code> instance passed as <code>constraint</code>.
     *
     * @param rightSide the object to compare for equality with <code>leftSide</code>, passed to the constructor
     * @param constraint an implicit <code>Constraint</code> instance that enforces a relationship between types <code>L</code> and <code>R</code> and
     *    defines a way of calculating equality for objects of type <code>L</code>
     * @return true if the <code>leftSide</code> and <code>rightSide</code> objects are equal according to the passed <code>Constraint</code> instance.
     */
    def ===[R](rightSide: R)(implicit constraint: Constraint[L, R]): Boolean = constraint.areEqual(leftSide, rightSide)
  
    /**
     * Compare two objects for inequality, returning a <code>Boolean</code>, using the <code>Constraint</code> instance passed as <code>constraint</code>.
     *
     * @param rightSide the object to compare for inequality with <code>leftSide</code>, passed to the constructor
     * @param constraint an implicit <code>Constraint</code> instance that enforces a relationship between types <code>L</code> and <code>R</code> and
     *    defines a way of calculating equality for objects of type <code>L</code>
     * @return true if the <code>leftSide</code> and <code>rightSide</code> objects are <em>not</em> equal according to the passed <code>Constraint</code> instance.
     */
    def !==[R](rightSide: R)(implicit constraint: Constraint[L, R]): Boolean = !constraint.areEqual(leftSide, rightSide)
  
    /**
     * Determine whether a numeric object is within the passed <code>Spread</code>, returning a <code>Boolean</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>leftSide</code> 
     * @return true if the value passed to the constructor as <code>leftSide</code> is <em>not</em> within the <code>Spread</code> passed to this method.
     */
    def ===(spread: Spread[L]): Boolean = if (spread != null) spread.isWithin(leftSide) else leftSide == spread
  
    /**
     * Determine whether a numeric object is outside the passed <code>Spread</code>, returning a <code>Boolean</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>leftSide</code> 
     * @return true if the value passed to the constructor as <code>leftSide</code> is <em>not</em> within the <code>Spread</code> passed to this method.
     */
    def !==(spread: Spread[L]): Boolean = if (spread != null) !spread.isWithin(leftSide) else leftSide != spread
  }

  /**
   * Class used via an implicit conversion to enable any two objects to be compared with
   * <code>===</code> and <code>!==</code> with an <code>Option[String]</code> result and an enforced type constraint between
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
  class LegacyCheckingEqualizer[L](left: L) {
  
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
     * Compare two objects for equality, returning an <code>Option[String]</code>, using the <code>Constraint</code> instance passed as <code>constraint</code>.
     *
     * @param right the object to compare for equality with <code>left</code>, passed to the constructor
     * @param constraint an implicit <code>Constraint</code> instance that enforces a relationship between types <code>L</code> and <code>R</code> and
     *    defines a way of calculating equality for objects of type <code>L</code>
     * @return None if the <code>left</code> and <code>right</code> objects are equal according to the passed <code>Equality</code> type class.
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def ===[R](right: R)(implicit constraint: Constraint[L, R]): Option[String] = 
      if (constraint.areEqual(left, right))
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, right)
        Some(FailureMessages("didNotEqual", leftee, rightee))
      }
  
    /**
     * Compare two objects for inequality, returning an <code>Option[String]</code>, using the <code>Constraint</code> instance passed as <code>constraint</code>.
     *
     * @param right the object to compare for inequality with <code>left</code>, passed to the constructor
     * @param constraint an implicit <code>Constraint</code> instance that enforces a relationship between types <code>L</code> and <code>R</code> and
     *    defines a way of calculating equality for objects of type <code>L</code>
     * @return None if the <code>left</code> and <code>right</code> objects are <em>not</em> equal according to the passed <code>Equality</code> type class.
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def !==[R](right: R)(implicit constraint: Constraint[L, R]): Option[String] =
      if (!constraint.areEqual(left, right))
        None
      else {
        val (leftee, rightee) = getObjectsForFailureMessage(left, right)
        Some(FailureMessages("equaled", leftee, rightee))
      }
  
    /**
     * Determine whether a numeric object is within the passed <code>Spread</code>, returning an <code>Option[String]</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>left</code> 
     * @return None if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Spread</code> passed to this method, 
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def ===(spread: Spread[L]): Option[String] =
      if (spread == null) {
        if (left == null)
          None
        else {
          val (leftee, rightee) = getObjectsForFailureMessage(left, spread)
          Some(FailureMessages("equaled", leftee, rightee))
        }
      }
      else {
        if (spread.isWithin(left))
          None
        else
          Some(FailureMessages("wasNotPlusOrMinus", left, spread.pivot, spread.tolerance))
      }
  
    /**
     * Determine whether a numeric object is outside the passed <code>Spread</code>, returning an <code>Option[String]</code>.
     *
     * @param spread the <code>Spread</code> against which to compare the value passed to the constructor as <code>left</code> 
     * @return true if the value passed to the constructor as <code>left</code> is <em>not</em> within the <code>Spread</code> passed to this method.
     *    else returns an error message string wrapped in a <code>Some</code>.
     */
    def !==(spread: Spread[L]): Option[String] =
      if (spread == null) {
        if (left != null)
          None
        else {
          val (leftee, rightee) = getObjectsForFailureMessage(left, spread)
          Some(FailureMessages("equaled", leftee, rightee))
        }
      }
      else {
        if (if (spread != null) !spread.isWithin(left) else left != spread)
          None
        else
          Some(FailureMessages("wasPlusOrMinus", left, spread.pivot, spread.tolerance))
      }
  }

  /**
   * Returns an <code>Equality[A]</code> for any type <code>A</code> that determines equality
   * by first calling <code>.deep</code> on any <code>Array</code> (on either the left or right side),
   * then comparing the resulting objects with <code>==</code>.
   *
   * @return a default <code>Equality</code> for type <code>A</code>
   */
  def defaultEquality[A]: Equality[A] = Equality.default

  /**
   * Converts to an <a href="Equalizer.html"><code>Equalizer</code></a> that provides <code>===</code> and <code>!==</code> operators that
   * result in <code>Boolean</code> and enforce no type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtrait <a href="TripleEquals.html"><code>TripleEquals</code></a> and overriden as non-implicit by the other
   * subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToEqualizer[T](left: T): Equalizer[T]

  /**
   * Converts to a <a href="LegacyEqualizer.html"><code>LegacyEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators that
   * result in <code>Option[String]</code> and enforce no type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtrait <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a> and overriden as non-implicit
   * by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>LegacyEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T]

  /**
   * Converts to an <a href="CheckingEqualizer.html"><code>CheckingEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators
   * that result in <code>Boolean</code> and enforce a type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtraits <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>, and overriden as
   * non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>CheckingEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T]

  /**
   * Converts to a <a href="LegacyCheckingEqualizer.html"><code>LegacyCheckingEqualizer</code></a> that provides <code>===</code> and <code>!==</code> operators
   * that result in <code>Option[String]</code> and enforce a type constraint.
   *
   * <p>
   * This method is overridden and made implicit by subtraits <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>
   * and <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param left the object whose type to convert to <code>LegacyCheckingEqualizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T]

  /**
   * Provides a <code>Constraint[A, B]</code> class for any two types <code>A</code> and <code>B</code>, with no type constraint enforced, given an
   * implicit <code>Equality[A]</code>.
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equality[A]</code>'s
   * <code>areEqual</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits <a href="TripleEquals.html"><code>TripleEquals</code></a> and
   * <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfA an <code>Equality[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEqual</code> method of
   *     the passed <code>Equality[A]</code>.
   */
  def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> for any two types <code>A</code> and <code>B</code>, enforcing the type constraint
   * that <code>A</code> must be a subtype of <code>B</code>, given an implicit <code>Equivalence[B]</code>.
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityTypeCheckedConstraint.html"><code>LowPriorityTypeCheckedConstraint</code></a> (extended by
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>), and
   * <a href="LowPriorityTypeCheckedLegacyConstraint.html"><code>LowPriorityTypeCheckedLegacyConstraint</code></a> (extended by
   * <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method
   *    will delegate to determine equality.
   * @param ev evidence that <code>A</code> is a subype of </code>B</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the
   * <code>areEquivalent</code> method of the passed <code>Equivalence[B]</code>.
   */
  def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> for any two types <code>A</code> and <code>B</code>, enforcing the type constraint
   * that <code>A</code> must be a subtype of <code>B</code>, given an <em>explicit</em> <code>Equivalence[B]</code>.
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[B]</code>, but
   * taking an implicit function that provides evidence that <code>A</code> is a subtype of </code>B</code>.
   * </p>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[B]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityTypeCheckedConstraint.html"><code>LowPriorityTypeCheckedConstraint</code></a> (extended by
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>), and
   * <a href="LowPriorityTypeCheckedLegacyConstraint.html"><code>LowPriorityTypeCheckedLegacyConstraint</code></a> (extended by
   * <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method
   *    will delegate to determine equality.
   * @param ev evidence that <code>A</code> is a subype of </code>B</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the
   * <code>areEquivalent</code> method of the passed <code>Equivalence[B]</code>.
   */
  def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> for any two types <code>A</code> and <code>B</code>, enforcing the type constraint
   * that <code>B</code> must be a subtype of <code>A</code>, given an implicit <code>Equivalence[A]</code>.
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>) and
   * <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param ev evidence that <code>B</code> is a subype of </code>A</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> for any two types <code>A</code> and <code>B</code>, enforcing the type constraint
   * that <code>B</code> must be a subtype of <code>A</code>, given an <em>explicit</em> <code>Equivalence[A]</code>.
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[B]</code>, but
   * taking an implicit function that provides evidence that <code>A</code> is a subtype of </code>B</code>. For example, under <code>TypeCheckedTripleEquals</code>,
   * this method (as an implicit method), would be used to compile this statement:
   * </p>
   *
   * <pre class="stHighlight">
   * def closeEnoughTo1(num: Double): Boolean =
   *   (num === 1.0)(decided by forgivingEquality)
   * </pre>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>) and
   * <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param ev evidence that <code>B</code> is a subype of </code>A</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> class for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>A</code> is
   * implicitly convertible to <code>B</code>, given an implicit <code>Equivalence[B]</code>.
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[B]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityConversionCheckedConstraint.html"><code>LowPriorityConversionCheckedConstraint</code></a> (extended by
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>), and
   * <a href="LowPriorityConversionCheckedLegacyConstraint.html"><code>LowPriorityConversionCheckedLegacyConstraint</code></a> (extended by
   * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>A</code> to </code>B</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[B]</code>.
   */
  def lowPriorityConversionCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], cnv: A => B): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> class for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>A</code> is
   * implicitly convertible to <code>B</code>, given an <em>explicit</em> <code>Equivalence[B]</code>.
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[B]</code>, but
   * taking an implicit function that converts from <code>A</code> to </code>B</code>.
   * </p>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[B]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityConversionCheckedConstraint.html"><code>LowPriorityConversionCheckedConstraint</code></a> (extended by
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>), and
   * <a href="LowPriorityConversionCheckedLegacyConstraint.html"><code>LowPriorityConversionCheckedLegacyConstraint</code></a> (extended by
   * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>A</code> to </code>B</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[B]</code>.
   */
  def convertEquivalenceToAToBConversionConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A => B): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> class for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>B</code> is
   * implicitly convertible to <code>A</code>, given an implicit <code>Equivalence[A]</code>.
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>) and
   * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>B</code> to </code>A</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): Constraint[A, B]

  /**
   * Provides a <code>Constraint[A, B]</code> class for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>B</code> is
   * implicitly convertible to <code>A</code>, given an <em>explicit</em> <code>Equivalence[A]</code>.
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[A]</code>, but
   * taking an implicit function that converts from <code>B</code> to </code>A</code>. For example, under <code>ConversionCheckedTripleEquals</code>,
   * this method (as an implicit method), would be used to compile this statement:
   * </p>
   *
   * <pre class="stHighlight">
   * def closeEnoughTo1(num: Double): Boolean =
   *   (num === 1.0)(decided by forgivingEquality)
   * </pre>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>) and
   * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>, and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>B</code> to </code>A</code>
   * @return a <code>Constraint[A, B]</code> whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): Constraint[A, B]

  /**
   * Returns a <code>TripleEqualsInvocation[T]</code>, given an object of type <code>T</code>, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should === <em>&lt;right&gt;</em></code>&rdquo; syntax
   * of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right the right-hand side value for an equality assertion
   * @return a <code>TripleEqualsInvocation</code> wrapping the passed <em>right</em> value, with <code>expectingEqual</code>
   *    set to <code>true</code>.
   */
  def ===[T](right: T): TripleEqualsInvocation[T] = new TripleEqualsInvocation[T](right, true)

  /**
   * Returns a <code>TripleEqualsInvocation[T]</code>, given an object of type <code>T</code>, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should !== <em>&lt;right&gt;</em></code>&rdquo; syntax
   * of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right the right-hand side value for an equality assertion
   * @return a <code>TripleEqualsInvocation</code> wrapping the passed <em>right</em> value, with <code>expectingEqual</code>
   *    set to <code>false</code>.
   */
  def !==[T](right: T): TripleEqualsInvocation[T] = new TripleEqualsInvocation[T](right, false)

  /**
   * Returns a <code>TripleEqualsInvocation[Null]</code>, given a <code>null</code> reference, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should === null</code>&rdquo; syntax
   * of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right a null reference
   * @return a <code>TripleEqualsInvocation</code> wrapping the passed <code>null</code> value, with <code>expectingEqual</code>
   *    set to <code>true</code>.
   */
  def ===(right: Null): TripleEqualsInvocation[Null] = new TripleEqualsInvocation[Null](right, true)

  /**
   * Returns a <code>TripleEqualsInvocation[Null]</code>, given a <code>null</code> reference, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should !== null</code>&rdquo; syntax
   * of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right a null reference
   * @return a <code>TripleEqualsInvocation</code> wrapping the passed <code>null</code> value, with <code>expectingEqual</code>
   *    set to <code>false</code>.
   */
  def !==(right: Null): TripleEqualsInvocation[Null] = new TripleEqualsInvocation[Null](right, false)

  /**
   * Returns a <code>TripleEqualsInvocationOnSpread[T]</code>, given an <code>Spread[T]</code>, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should === (<em>&lt;pivot&gt;</em> +- <em>&lt;tolerance&gt;</em>)</code>&rdquo;
   * syntax of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right the <code>Spread[T]</code> against which to compare the left-hand value
   * @return a <code>TripleEqualsInvocationOnSpread</code> wrapping the passed <code>Spread[T]</code> value, with
   * <code>expectingEqual</code> set to <code>true</code>.
   */
  def ===[T](right: Spread[T]): TripleEqualsInvocationOnSpread[T] = new TripleEqualsInvocationOnSpread[T](right, true)

  /**
   * Returns a <code>TripleEqualsInvocationOnSpread[T]</code>, given an <code>Spread[T]</code>, to facilitate
   * the &ldquo;<code><em>&lt;left&gt;</em> should !== (<em>&lt;pivot&gt;</em> +- <em>&lt;tolerance&gt;</em>)</code>&rdquo;
   * syntax of <a href="../scalatest/Matchers.html"><code>Matchers</code></a>.
   *
   * @param right the <code>Spread[T]</code> against which to compare the left-hand value
   * @return a <code>TripleEqualsInvocationOnSpread</code> wrapping the passed <code>Spread[T]</code> value, with
   * <code>expectingEqual</code> set to <code>false</code>.
   */
  def !==[T](right: Spread[T]): TripleEqualsInvocationOnSpread[T] = new TripleEqualsInvocationOnSpread[T](right, false)
}

object TripleEqualsSupport {

  /**
   * An implementation of <a href="Constraint.html"><code>Constraint</code></a> for two types <code>A</code> and <code>B</code> that requires an <code>Equality[A]</code> to
   * which its <code>areEqual</code> method can delegate an equality comparison.
   *
   * @param equalityOfA an <code>Equality</code> type class for <code>A</code>
   */
  final class EqualityConstraint[A, B](equalityOfA: Equality[A]) extends Constraint[A, B] {

    /**
     * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by returning the
     * result of invoking <code>areEqual(a, b)</code> on the passed <code>equalityOfA</code> object.
     *
     * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     */
    def areEqual(a: A, b: B): Boolean = equalityOfA.areEqual(a, b)
  }

  /**
   * An implementation of <code>Constraint</code> for two types <code>A</code> and <code>B</code> that requires an <code>Equality[B]</code>
   * and a conversion function from <code>A</code> to <code>B</code>. 
   *
   * @param equivalenceOfB an <code>Equivalence</code> type class for <code>B</code>
   */
  final class AToBEquivalenceConstraint[A, B](equivalenceOfB: Equivalence[B], cnv: A => B) extends Constraint[A, B] {

    /**
     * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by return the
     * result of invoking <code>areEqual(cnv(a), b)</code> on the passed <code>equalityOfB</code> object.
     *
     * <p>
     * In other words, the <code>a</code> object of type <code>A</code> is first converted to a <code>B</code> via the passed conversion
     * function, <code>cnv</code>, then compared for equality with the <code>b</code> object.
     * </p>
     *
     * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     */
    override def areEqual(a: A, b: B): Boolean = equivalenceOfB.areEquivalent(cnv(a), b)
  }
  
  /**
   * An implementation of <code>Constraint</code> for two types <code>A</code> and <code>B</code> that requires an <code>Equality[A]</code>
   * and a conversion function from <code>B</code> to <code>A</code>. 
   *
   * @param equivalenceOfA an <code>Equivalence</code> type class for <code>A</code>
   */
  final class BToAEquivalenceConstraint[A, B](equivalenceOfA: Equivalence[A], cnv: B => A) extends Constraint[A, B] {
  
    /**
     * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by returning the
     * result of invoking <code>areEqual(a, cnv(b))</code> on the passed <code>equalityOfA</code> object.
     *
     * <p>
     * In other words, the <code>b</code> object of type <code>B</code> is first converted to an <code>A</code> via the passed conversion
     * function, <code>cnv</code>, then compared for equality with the <code>a</code> object.
     * </p>
     *
     * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
     */
    override def areEqual(a: A, b: B): Boolean = equivalenceOfA.areEquivalent(a, cnv(b))
  }

  /**
   * Facilitates the &ldquo;<code>should === (x += y)</code>&rdquo; and  &ldquo;<code>should !== (x += y)</code>&rdquo; syntax of ScalaTest's matchers DSL. 
   *  
   * <p>
   * Instances of this class are created and returned by the <code>===</code> and <code>!==</code> methods of
   * trait <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>.
   * </p>
   *
   * @param spread the <code>Spread[T]</code> against which to compare the left-hand value
   * @param expectingEqual <code>true</code> if the result of a <code>===</code> invocation; <code>false</code> if the result of a <code>!==</code> invocation.
   */
  final case class TripleEqualsInvocationOnSpread[T](spread: Spread[T], expectingEqual: Boolean)
  
  /**
   * Facilitates the &ldquo;<code>should ===</code>&rdquo; and  &ldquo;<code>should !==</code>&rdquo; syntax of ScalaTest's matchers DSL. 
   *  
   * <p>
   * Instances of this class are created and returned by the <code>===</code> and <code>!==</code> methods of
   * trait <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>.
   * </p>
   *
   * @param right the right-hand side value for an equality assertion
   * @param expectingEqual <code>true</code> if the result of a <code>===</code> invocation; <code>false</code> if the result of a <code>!==</code> invocation.
   */
  final case class TripleEqualsInvocation[T](right: T, expectingEqual: Boolean) {
    override def toString: String = (if (expectingEqual) "===" else "!==") + " " + Prettifier.default(right)
  }

  /**
   * Class representing an spread (<em>i.e.</em>, range) between two numbers.
   * 
   * <p>
   * The spread is expressed in terms of a <code>Numeric</code> <em>pivot</em> and <em>tolerance</em>.
   * The spread extends from <code>pivot - tolerance</code> to <code>pivot + tolerance</code>, inclusive.
   * </p>
   * 
   * @param pivot the pivot number at the center of the spread
   * @param tolerance the tolerance that determines the high and low point of the spread
   * 
   * @author Bill Venners
   */
  final case class Spread[T : Numeric](pivot: T, tolerance: T) {
  
    private val numeric = implicitly[Numeric[T]]
  
    require(numeric.signum(tolerance) >= 0, "tolerance must be zero or greater, but was " + tolerance)
  
    private val max = numeric.plus(pivot, tolerance)
    private val min = numeric.minus(pivot, tolerance)
  
    /**
     * Determines whether the passed <code>Numeric</code> value <code>n</code> is within the spread represented
     * by this <code>Spread</code> instance.
     */
    def isWithin(n: T): Boolean = {
      numeric.gteq(n, min) && numeric.lteq(n, max)
    }
  
    /**
     * Returns <code>true</code> if the passed number, <code>n</code>, is within the spread represented by this <code>Spread</code> instance
     *
     * <p>
     * The purpose of this method, which will likely be used only rarely, is to achieve symmetry around the <code>===</code> operator. The
     * <code>TripleEquals</code> trait (and its type-checking siblings <code>TypeCheckedTripleEquals</code> and <code>ConversionCheckedTripleEquals</code>) enable you to write:
     * </p>
     *
     * <pre>
     * a === (1.0 +- 0.1)
     * </pre>
     *
     * <p>
     * This method ensures the following mirrored form means the same thing:
     * </p>
     *
     * <pre>
     * (1.0 +- 0.1) === a
     * </pre>
     *
     * @param n a number that may or may not lie within this spread
     */
    def ===(n: T): Boolean = isWithin(n)
  
    /**
     * Returns <code>false</code> if the passed number, <code>n</code>, is within the spread represented by this <code>Spread</code> instance
     *
     * <p>
     * The purpose of this method, which will likely be used only rarely, is to achieve symmetry around the <code>!==</code> operator. The
     * <code>TripleEquals</code> trait (and its type-checking siblings <code>TypeCheckedTripleEquals</code> and <code>ConversionCheckedTripleEquals</code>) enable you to write:
     * </p>
     *
     * <pre>
     * a !== (1.0 +- 0.1)
     * </pre>
     *
     * <p>
     * This method ensures the following mirrored form means the same thing:
     * </p>
     *
     * <pre>
     * (1.0 +- 0.1) !== a
     * </pre>
     *
     * @param n a number that may or may not lie within this spread
     */
    def !==(n: T): Boolean = !isWithin(n)
    
    /**
     * Overrides toString to return "[pivot] plusOrMinus [tolerance]"
     */
    override def toString: String = Prettifier.default(pivot) + " +- " + Prettifier.default(tolerance)
  }
}
