/*
 * Copyright 2001-2025 Artima, Inc.
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

import scala.reflect.ClassTag

/**
 * Trait extended by matcher objects, which may appear after the word <code>be</code>, that can match a value of the specified type.
 * The value to match is passed to the <code>BeMatcher</code>'s <code>apply</code> method. The result is a <code>MatchResult</code>.
 * A <code>BeMatcher</code> is, therefore, a function from the specified type, <code>T</code>, to a <code>MatchResult</code>.
 *
 * <p>
 * Although <code>BeMatcher</code>
 * and <code>Matcher</code> represent very similar concepts, they have no inheritance relationship
 * because <code>Matcher</code> is intended for use right after <code>should</code> or <code>must</code>
 * whereas <code>BeMatcher</code> is intended for use right after <code>be</code>.
 * </p>
 *
 * <p>
 * As an example, you could create <code>BeMatcher[Int]</code>
 * called <code>odd</code> that would match any odd <code>Int</code>, and one called <code>even</code> that would match
 * any even <code>Int</code>. 
 * Given this pair of <code>BeMatcher</code>s, you could check whether an <code>Int</code> was odd or even with expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * num should be (odd)
 * num should not be (even)
 * </pre>
 *
 * <p>
 * Here's is how you might define the odd and even <code>BeMatchers</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * trait CustomMatchers {
 *
 *   class OddMatcher extends BeMatcher[Int] {
 *     def apply(left: Int) =
 *       MatchResult(
 *         left % 2 == 1,
 *         left.toString + " was even",
 *         left.toString + " was odd"
 *       )
 *   }
 *   val odd = new OddMatcher
 *   val even = not (odd)
 * }
 *
 * // Make them easy to import with:
 * // import CustomMatchers._
 * object CustomMatchers extends CustomMatchers
 * </pre>
 *
 * <p>
 * These <code>BeMatcher</code>s are defined inside a trait to make them easy to mix into any
 * suite or spec that needs them.
 * The <code>CustomMatchers</code> companion object exists to make it easy to bring the
 * <code>BeMatcher</code>s defined in this trait into scope via importing, instead of mixing in the trait. The ability
 * to import them is useful, for example, when you want to use the matchers defined in a trait in the Scala interpreter console.
 * </p>
 *
 * <p>
 * Here's an rather contrived example of how you might use <code>odd</code> and <code>even</code>: 
 * </p>
 *
 * <pre class="stHighlight">
 * class DoubleYourPleasureSuite extends FunSuite with MustMatchers with CustomMatchers {
 *
 *   def doubleYourPleasure(i: Int): Int = i * 2
 *
 *   test("The doubleYourPleasure method must return proper odd or even values")
 *
 *     val evenNum = 2
 *     evenNum must be (even)
 *     doubleYourPleasure(evenNum) must be (even)
 *
 *     val oddNum = 3
 *     oddNum must be (odd)
 *     doubleYourPleasure(oddNum) must be (odd) // This will fail
 *   }
 * }
 * </pre>
 *
 * <p>
 * The last assertion in the above test will fail with this failure message:
 * </p>
 *
 * <pre class="stHighlight">
 * 6 was even
 * </pre>
 *
 * <p>
 * For more information on <code>MatchResult</code> and the meaning of its fields, please
 * see the documentation for <a href="MatchResult.html"><code>MatchResult</code></a>. To understand why <code>BeMatcher</code>
 * is contravariant in its type parameter, see the section entitled "Matcher's variance" in the
 * documentation for <a href="../Matcher.html"><code>Matcher</code></a>.
 * </p>
 *
 * @author Bill Venners
*/
trait BeMatcher[-T] extends Function1[T, MatchResult] { thisBeMatcher =>

  /**
   * Check to see if the specified object, <code>left</code>, matches, and report the result in
   * the returned <code>MatchResult</code>. The parameter is named <code>left</code>, because it is
   * usually the value to the left of a <code>should</code> or <code>must</code> invocation. For example,
   * in:
   *
   * <pre class="stHighlight">
   * num should be (odd)
   * </pre>
   *
   * The <code>be (odd)</code> expression results in a regular <a href="../Matcher.html"><code>Matcher</code></a> that holds
   * a reference to <code>odd</code>, the
   * <code>BeMatcher</code> passed to <code>be</code>. The <code>should</code> method invokes <code>apply</code>
   * on this matcher, passing in <code>num</code>, which is therefore the "<code>left</code>" value. The
   * matcher will pass <code>num</code> (the <code>left</code> value) to the <code>BeMatcher</code>'s <code>apply</code>
   * method.
   *
   * @param left the value against which to match
   * @return the <code>MatchResult</code> that represents the result of the match
   */
  def apply(left: T): MatchResult

  /**
   * Compose this <code>BeMatcher</code> with the passed function, returning a new <code>BeMatcher</code>.
   *
   * <p>
   * This method overrides <code>compose</code> on <code>Function1</code> to
   * return a more specific function type of <code>BeMatcher</code>. For example, given
   * an <code>odd</code> matcher defined like this:
   * </p>
   *
   * <pre class="stHighlight">
   * val odd =
   *   new BeMatcher[Int] {
   *     def apply(left: Int) =
   *       MatchResult(
   *         left % 2 == 1,
   *         left.toString + " was even",
   *         left.toString + " was odd"
   *       )
   *   }
   * </pre>
   *
   * <p>
   * You could use <code>odd</code> like this:
   * </p>
   *
   * <pre class="stHighlight">
   * 3 should be (odd)
   * 4 should not be (odd)
   * </pre>
   *
   * <p>
   * If for some odd reason, you wanted a <code>BeMatcher[String]</code> that 
   * checked whether a string, when converted to an <code>Int</code>,
   * was odd, you could make one by composing <code>odd</code> with
   * a function that converts a string to an <code>Int</code>, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * val oddAsInt = odd compose { (s: String) => s.toInt }
   * </pre>
   *
   * <p>
   * Now you have a <code>BeMatcher[String]</code> whose <code>apply</code> method first
   * invokes the converter function to convert the passed string to an <code>Int</code>,
   * then passes the resulting <code>Int</code> to <code>odd</code>. Thus, you could use
   * <code>oddAsInt</code> like this:
   * </p>
   *
   * <pre class="stHighlight">
   * "3" should be (oddAsInt)
   * "4" should not be (oddAsInt)
   * </pre>
   */
  override def compose[U](g: U => T): BeMatcher[U] =
    new BeMatcher[U] {
      def apply(u: U) = thisBeMatcher.apply(g(u))
    }
}

/**
 * Companion object for trait <code>BeMatcher</code> that provides a
 * factory method that creates a <code>BeMatcher[T]</code> from a
 * passed function of type <code>(T =&gt; MatchResult)</code>.
 *
 * @author Bill Venners
 */
object BeMatcher {

  /**
   * Factory method that creates a <code>BeMatcher[T]</code> from a
   * passed function of type <code>(T =&gt; MatchResult)</code>.
   *
   * @author Bill Venners
   */
  def apply[T](fun: T => MatchResult)(implicit ev: ClassTag[T]): BeMatcher[T] =
    new BeMatcher[T] {
      def apply(left: T) = fun(left)
      override def toString: String = "BeMatcher[" + ev.runtimeClass.getName + "](" + ev.runtimeClass.getName + " => MatchResult)"
    }
}

