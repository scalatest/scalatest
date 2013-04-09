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
package org.scalatest.words

import org.scalatest.matchers._
import org.scalautils._
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.Suite
import org.scalatest.Assertions.areEqualComparingArraysStructurally
import org.scalatest.Matchers.matchSymbolToPredicateMethod

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="ShouldMatchers.html"><code>ShouldMatchers</code></a> or <a href="MustMatchers.html"><code>MustMatchers</code></a> for an overview of
 * the matchers DSL.
 *
 * <p>
 * Class <code>BeWord</code> contains an <code>apply</code> method that takes a <code>Symbol</code>, which uses reflection
 * to find and access a <code>Boolean</code> property and determine if it is <code>true</code>.
 * If the symbol passed is <code>'empty</code>, for example, the <code>apply</code> method
 * will use reflection to look for a public Java field named
 * "empty", a public method named "empty", or a public method named "isEmpty". If a field, it must be of type <code>Boolean</code>.
 * If a method, it must take no parameters and return <code>Boolean</code>. If multiple candidates are found,
 * the <code>apply</code> method will select based on the following algorithm:
 * </p>
 * 
 * <table class="stTable">
 * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"is" Method</th><th class="stHeadingCell">Result</th></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>empty</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
 * </table>
 * 
 * @author Bill Venners
 */
final class BeWord {


  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &lt; (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the less than operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the less than operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &lt; (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &lt; (7))
   *                       ^
   * </pre>
   */
  def <[T <% Ordered[T]](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          left < right,
          FailureMessages("wasNotLessThan", left, right),
          FailureMessages("wasLessThan", left, right)
        )
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &gt; (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the greater than operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the greater than operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &gt; (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &gt; (7))
   *                       ^
   * </pre>
   */
  def >[T <% Ordered[T]](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          left > right,
          FailureMessages("wasNotGreaterThan", left, right),
          FailureMessages("wasGreaterThan", left, right)
        )
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &lt;= (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the less than or equal to operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the less than or equal to operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &lt;= (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &lt;= (7))
   *                       ^
   * </pre>
   */
  def <=[T <% Ordered[T]](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          left <= right,
          FailureMessages("wasNotLessThanOrEqualTo", left, right),
          FailureMessages("wasLessThanOrEqualTo", left, right)
        )
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &gt;= (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the greater than or equal to operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the greater than or equal to operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &gt;= (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &gt;= (7))
   *                       ^
   * </pre>
   */
  def >=[T <% Ordered[T]](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult =
        MatchResult(
          left >= right,
          FailureMessages("wasNotGreaterThanOrEqualTo", left, right),
          FailureMessages("wasGreaterThanOrEqualTo", left, right)
        )
    }


  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should be === (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the === operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the ===n operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be === (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be === (7))
   *                       ^
   * </pre>
   */
  def ===(right: Any): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        MatchResult(
          areEqualComparingArraysStructurally(left, right),
          FailureMessages("wasNotEqualTo", leftee, rightee),
          FailureMessages("wasEqualTo", left, right)
        )
      }
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * fileMock should not { be a ('file) }
   *                          ^
   * </pre>
   */
  def a[S <: AnyRef](right: Symbol): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, true)
    }

  /**
   * This method enables the following syntax, where <code>fileMock</code> is, for example, of type <code>File</code> and
   * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
   *
   * <pre class="stHighlight">
   * fileMock should not { be a (file) }
   *                          ^
   * </pre>
   */
  def a[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          FailureMessages("wasNotA", left, UnquotedString(result.propertyName)), 
          FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }
  
  /**
   * This method enables the following syntax, where <code>negativeNumber</code> is, for example, of type <code>AMatcher</code>:
   *
   * <pre class="stHighlight">
   * 8 should not { be a (negativeNumber) }
   *                   ^
   * </pre>
   */
  def a[S](aMatcher: AMatcher[S]): Matcher[S] = 
    new Matcher[S] {
      def apply(left: S): MatchResult = aMatcher(left)
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * animal should not { be an ('elephant) }
   *                        ^
   * </pre>
   */
  def an[S <: AnyRef](right: Symbol): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, true, false)
    }

  /**
   * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
   * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
   *
   * <pre class="stHighlight">
   * keyEvent should not { be an (actionKey) }
   *                          ^
   * </pre>
   */
  def an[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          FailureMessages("wasNotAn", left, UnquotedString(result.propertyName)),
          FailureMessages("wasAn", left, UnquotedString(result.propertyName))
        )
      }
    }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> is, for example, of type <code>AnMatcher</code>:
   *
   * <pre class="stHighlight">
   * 8 should not { be an (oddNumber) }
   *                   ^
   * </pre>
   */
  def an[S](anMatcher: AnMatcher[S]): Matcher[S] = 
    new Matcher[S] {
      def apply(left: S): MatchResult = anMatcher(left)
    }

  /**
   * This method enables the following syntax for the "primitive" numeric types: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should be (7.1 plusOrMinus 0.2)
   *                      ^
   * </pre>
   */
  def apply[U](interval: Interval[U]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        MatchResult(
          interval.isWithin(left),
          // left <= right + tolerance && left >= right - tolerance,
          FailureMessages("wasNotPlusOrMinus", left, interval.pivot, interval.tolerance),
          FailureMessages("wasPlusOrMinus", left, interval.pivot, interval.tolerance)
        )
      }
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be theSameInstancreAs (anotherObject)
   *                  ^
   * </pre>
   */
  def theSameInstanceAs(right: AnyRef): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult =
        MatchResult(
          left eq right,
          FailureMessages("wasNotSameInstanceAs", left, right),
          FailureMessages("wasSameInstanceAs", left, right)
        )
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be (true)
   *                  ^
   * </pre>
   */
  def apply(right: Boolean): Matcher[Boolean] = 
    new Matcher[Boolean] {
      def apply(left: Boolean): MatchResult =
        MatchResult(
          left == right,
          FailureMessages("wasNot", left, right),
          FailureMessages("was", left, right)
        )
    }

/* Well heck if I don't need this one
    [fsc] both method apply in class BeWord of type [T](org.scalatest.BePropertyMatcher[T])org.scalatest.Matcher[T]
    [fsc] and  method apply in class BeWord of type [T](org.scalatest.BeMatcher[T])org.scalatest.Matcher[T]
    [fsc] match argument types (Null)
    [fsc]         o should be (null)
    [fsc]                  ^
*/

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be (null)
   *                  ^
   * </pre>
   */
  def apply(o: Null): Matcher[AnyRef] = 
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          FailureMessages("wasNotNull", left),
          FailureMessages("wasNull"),
          FailureMessages("wasNotNull", left),
          FailureMessages("midSentenceWasNull")
        )
      }
    }

  /* *
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should be ('empty)
   *               ^
   * </pre>
  def apply[T](right: AType[T]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = 
        MatchResult(
          right.isAssignableFromClassOf(left),
          FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)),
          FailureMessages("wasAnInstanceOf"), // TODO, missing the left, right.className here. Write a test and fix it.
          FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)),
          FailureMessages("wasAnInstanceOf")
        )
    }
   */

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should be ('empty)
   *               ^
   * </pre>
   */
  def apply[S <: AnyRef](right: Symbol): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = matchSymbolToPredicateMethod[S](left, right, false, false)
    }

  /**
   * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
   * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * num should be (odd)
   *               ^
   * </pre>
   */
  def apply[T](right: BeMatcher[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = right(left)
    }

  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * door should be (open)
   *                ^
   * </pre>
   */
  def apply[T](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          FailureMessages("wasNot", left, UnquotedString(result.propertyName)), 
          FailureMessages("was", left, UnquotedString(result.propertyName))
        )
      }
    }

  /**
   * This method enables <code>be</code> to be used for equality comparison. Here are some examples: 
   *
   * <pre class="stHighlight">
   * result should be (None)
   *                  ^
   * result should be (Some(1))
   *                  ^
   * result should be (true)
   *                  ^
   * result should be (false)
   *                  ^
   * sum should be (19)
   *               ^
   * </pre>
   */
  def apply(right: Any): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult =
        left match {
          case null =>
            MatchResult(
              right == null,
              FailureMessages("wasNotNull", right),
              FailureMessages("wasNull"),
              FailureMessages("wasNotNull", right),
              FailureMessages("midSentenceWasNull")
            )
          case _ => {
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
            MatchResult(
              areEqualComparingArraysStructurally(left, right),
              FailureMessages("wasNotEqualTo", leftee, rightee),
              FailureMessages("wasEqualTo", left, right)
            )
          }
      }
    }
}
