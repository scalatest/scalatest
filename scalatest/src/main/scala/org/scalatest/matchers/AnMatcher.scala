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
package org.scalatest.matchers

import org.scalatest._
import org.scalactic.Prettifier

/**
 * Trait extended by matcher objects that can match a value of the specified type.
 * <code>AnMatcher</code> represents a noun that appears after the word <code>an</code>, thus a nounName is required.
 *
 * <p>
 * The value to match is passed to the <code>AnMatcher</code>'s <code>apply</code> method. The result is a <code>MatchResult</code>.
 * An <code>AnMatcher</code> is, therefore, a function from the specified type, <code>T</code>, to a <code>MatchResult</code>.
 * </p>
 *
 * <p>
 * Although <code>AnMatcher</code>
 * and <code>Matcher</code> represent very similar concepts, they have no inheritance relationship
 * because <code>Matcher</code> is intended for use right after <code>should</code> or <code>must</code>
 * whereas <code>AnMatcher</code> is intended for use right after <code>an</code>.
 * </p>
 *
 * <p>
 * As an example, you could create <code>AnMatcher[Int]</code>
 * called <code>oddNumber</code> that would match any odd <code>Int</code>, and one called <code>evenNumber</code> that would match
 * any even <code>Int</code>.
 * Given this pair of <code>AnMatcher</code>s, you could check whether an <code>Int</code> was odd or even number with expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * num should be an oddNumber
 * num should not be an evenNumber
 * </pre>
 *
 * <p>
 * Here's is how you might define the oddNumber and evenNumber <code>AnMatchers</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // Using AnMatcher.apply method
 * val oddNumber = AnMatcher[Int]("odd number"){ _ % 2 != 0 }
 *
 * // Or by extending AnMatcher trait
 * val evenNumber = new AnMatcher[Int] {
 *   val nounName = "even number"
 *   def apply(left: Int): MatchResult =
 *     MatchResult(
 *       left % 2 == 0,
 *       left + " was not an " + nounName,
 *       left + " was an " + nounName
 *     )
 * }
 * </pre>
 *
 * <p>
 * Here's an rather contrived example of how you might use <code>oddNumber</code> and <code>evenNumber</code>:
 * </p>
 *
 * <pre class="stHighlight">
 *
 * val num1 = 1
 * num1 should be an oddNumber
 *
 * val num2 = num1 + 1
 * num2 should be an evenNumber
 *
 * num1 should be an evenNumber
 * </pre>
 *
 * <p>
 * The last assertion in the above test will fail with this failure message:
 * </p>
 *
 * <pre class="stHighlight">
 * 1 was not an even number
 * </pre>
 *
 * <p>
 * For more information on <code>MatchResult</code> and the meaning of its fields, please
 * see the documentation for <a href="MatchResult.html"><code>MatchResult</code></a>. To understand why <code>AnMatcher</code>
 * is contravariant in its type parameter, see the section entitled "Matcher's variance" in the
 * documentation for <a href="Matcher.html"><code>Matcher</code></a>.
 * </p>
 *
 * @tparam T The type used by this AnMatcher's apply method.
 * @author Bill Venners
 * @author Chee Seng
 */
private[scalatest] trait AnMatcher[-T] extends Function1[T, MatchResult] {
  /**
   * The name of the noun that this <code>AnMatcher</code> represents.
   */
  val nounName: String

  /**
   * Check to see if the specified object, <code>left</code>, matches, and report the result in
   * the returned <code>MatchResult</code>. The parameter is named <code>left</code>, because it is
   * usually the value to the left of a <code>should</code> or <code>must</code> invocation. For example,
   * in:
   *
   * <pre class="stHighlight">
   * num should be an oddNumber
   * </pre>
   *
   * The <code>num should be</code> expression results in a regular <a href="../Matchers$ResultOfBeWordForAny.html"><code>ResultOfBeWordForAny</code></a> that hold
   * a reference to <code>num</code> and has a method named <code>an</code> that takes a <code>AnMatcher</code>.  The <code>an</code> method
   * calls <code>AnMatcher</code>'s apply method by passing in the <code>num</code>, and check if <code>num</code> matches.
   *
   * @param left the value against which to match
   * @return the <code>MatchResult</code> that represents the result of the match
   */
  def apply(left: T): MatchResult
}

/**
 * Companion object for trait <code>AnMatcher</code> that provides a
 * factory method that creates a <code>AnMatcher[T]</code> from a
 * passed noun name and function of type <code>(T =&gt; MatchResult)</code>.
 *
 * @author Bill Venners
 * @author Chee Seng
 */
private[scalatest] object AnMatcher {

  /**
   * Factory method that creates a <code>AnMatcher[T]</code> from a
   * passed noun name and function of type <code>(T =&gt; MatchResult)</code>.
   *
   * @param name the noun name
   * @param fun the function of type <code>(T =&gt; MatchResult)</code>
   * @return <code>AnMatcher</code> instance that has the passed noun name and matches using the passed function
   * @author Bill Venners
   * @author Chee Seng
   */
  def apply[T](name: String)(fun: T => Boolean)(implicit ev: Manifest[T]) = 
    new AnMatcher[T] {
      val nounName = name
      def apply(left: T): MatchResult = 
        MatchResult(
          fun(left), 
          Resources("wasNotAn"), 
          Resources("wasAn"), 
          Vector(left, UnquotedString(nounName))
        )
      override def toString: String = "AnMatcher[" + ev.erasure.getName + "](" + Prettifier.default(name) + ", " + ev.erasure.getName + " => Boolean)"
    }
  
}
