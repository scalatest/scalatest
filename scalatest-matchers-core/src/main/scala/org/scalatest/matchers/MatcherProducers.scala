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

/**
 * Provides an implicit conversion on functions that <em>produce</em> <code>Matcher</code>s, <em>i.e.</em>,
 * <code>T =&gt; Matcher[T]</code> that enables you to modify error messages when composing <code>Matcher</code>s.
 *
 * <p>
 * For an example of using <code>MatcherProducers</code>, see the <a href="Matcher.html#composingMatchers">Composing matchers</a>
 * section in the main documentation for trait <code>Matcher</code>.
 * </p>
 *
 * @author Bill Venners
 * @author Chee Seng
 */
trait MatcherProducers {

  /**
   * Class used via an implicit conversion that adds <code>composeTwice</code>, <code>mapResult</code>, and
   * <code>mapArgs</code> methods to functions that produce a <code>Matcher</code>.
   */
  class Composifier[T](f: T => Matcher[T]) {

    /**
     * Produces a new &ldquo;matcher producer&rdquo; function of type <code>U =&gt; Matcher[U]</code> from the
     * <code>T =&gt; Matcher[T]</code> (named <code>f</code>) passed to the <code>Composifier</code> constructor and the given
     * <code>T =&gt; U</code> transformation  function, <code>g</code>.
     *
     * <p>
     * The result of <code>composeTwice</code> is the result of the following function composition expression:
     * </p>
     *
     * <pre class="stHighlight">
     * (f compose g) andThen (_ compose g)
     * </pre>
     * 
     * <p>
     * You would use <code>composeTwice</code> if you want to create a new matcher producer from an existing one, by transforming
     * both the left and right sides of the expression with the same transformation function. As an example, the
     * expression &ldquo;<code>be &gt; 7</code>&rdquo; produces a <code>Matcher[Int]</code>:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; import org.scalatest._
     * import org.scalatest._
     *
     * scala&gt; import Matchers._
     * import Matchers._
     *
     * scala&gt; val beGreaterThanSeven = be &gt; 7
     * beGreaterThanSeven: org.scalatest.matchers.Matcher[Int] = be &gt; 7
     * </pre>
     *
     * <p>
     * Given this <code>Matcher[Int]</code>, you can now use it in a <code>should</code> expression like this:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; 8 should beGreaterThanSeven
     *
     * scala&gt; 6 should beGreaterThanSeven
     * org.scalatest.exceptions.TestFailedException: 6 was not greater than 7
     * ...
     * </pre>
     *
     * <p>
     * You can create a more general &ldquo;matcher producer&rdquo; function like this:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; val beGreaterThan = { (i: Int) =&gt; be &gt; i }
     * beGreaterThan: Int =&gt; org.scalatest.matchers.Matcher[Int] = &lt;function1&gt;
     *
     * scala&gt; 8 should beGreaterThan (7)
     *
     * scala&gt; 8 should beGreaterThan (9)
     * org.scalatest.exceptions.TestFailedException: 8 was not greater than 9
     * </pre>
     * 
     * <p>
     * Given <code>beGreaterThan</code> matcher producer function, you can create matcher producer function
     * that takes a <code>String</code> and produces a <code>Matcher[String]</code> given a function from
     * <code>Int =&gt; String</code> using <code>composeTwice</code>:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; val stringToInt = { (s: String) =&gt; s.toInt }
     * stringToInt: String =&gt; Int = &lt;function1&gt;
     *
     * scala&gt; val beAsIntsGreaterThan = beGreaterThan composeTwice stringToInt
     * beAsIntsGreaterThan: String =&gt; org.scalatest.matchers.Matcher[String] = &lt;function1&gt;
     *
     * scala&gt; "7" should beAsIntsGreaterThan ("6")
     *
     * scala&gt; "7" should beAsIntsGreaterThan ("8")
     * org.scalatest.exceptions.TestFailedException: 7 was not greater than 8
     * ...
     * </pre>
     * 
     * <p>
     * The <code>composeTwice</code> method is just a shorthand for this function composition expression:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; val beAsIntsGreaterThan =
     *     (beGreaterThan compose stringToInt) andThen (_ compose stringToInt)
     * beAsIntsGreaterThan: String =&gt; org.scalatest.matchers.Matcher[String] = &lt;function1&gt;
     *
     * scala&gt; "7" should beAsIntsGreaterThan ("6")
     *
     * scala&gt; "7" should beAsIntsGreaterThan ("8")
     * org.scalatest.exceptions.TestFailedException: 7 was not greater than 8
     * </pre>
     * 
     * <p>
     * The first part of that expression, <code>beGreaterThan</code> <code>compose</code> <code>stringToInt</code>,
     * gives you a new matcher producer function that given a <code>String</code> will produce a <code>Matcher[Int]</code>:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; val beAsIntGreaterThan = beGreaterThan compose stringToInt
     * beAsIntGreaterThan: String =&gt; org.scalatest.matchers.Matcher[Int] = &lt;function1&gt;
     * </pre>
     * 
     * <p>
     * This <code>compose</code> method is inherited from <code>Function1</code>: on any <code>Function1</code>,
     * <code>(f</code> <code>compose</code> <code>g)(x)</code> means <code>f(g(x))</code>. You can use this
     * matcher producer like this:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; 7 should beAsIntGreaterThan ("6")
     *
     * scala&gt; 7 should beAsIntGreaterThan ("8")
     * org.scalatest.exceptions.TestFailedException: 7 was not greater than 8
     * </pre>
     *
     * <p>
     * To get a matcher producer that will allow you to put a string on the right-hand-side, you'll need to transform
     * the <code>String</code> <code>=&gt;</code> <code>Matcher[Int]</code> to a <code>String</code> <code>=&gt;</code>
     * <code>Matcher[String]</code>. To accomplish this you can first just apply the function to get a <code>Matcher[Int]</code>,
     * like this:
     * </p>
     * 
     * <pre class="stREPL">
     * scala&gt; val beGreaterThanEight = beAsIntGreaterThan ("8")
     * beGreaterThanEight: org.scalatest.matchers.Matcher[Int] = be &gt; 8
     *
     * scala&gt; 9 should beGreaterThanEight
     *
     * scala&gt; 7 should beGreaterThanEight
     * org.scalatest.exceptions.TestFailedException: 7 was not greater than 8
     * </pre>
     *
     * <p>
     * To transform <code>beGreaterThanEight</code>, a <code>Matcher[Int]</code>, to a <code>Matcher[String]</code>,
     * you can again use <code>compose</code>. A ScalaTest <code>Matcher[T]</code> is a Scala function type <code>T</code>
     * <code>=&gt;</code> <code>MatchResult</code>. To get a <code>Matcher[String]</code> therefore, just call
     * <code>compose</code> on the <code>Matcher[Int]</code> and pass in a function from <code>String</code> <code>=&gt;</code>
     * <code>Int</code>:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; val beAsIntGreaterThanEight = beGreaterThanEight compose stringToInt
     * beAsIntGreaterThanEight: org.scalatest.matchers.Matcher[String] = &lt;function1&gt;
     * </pre>
     *
     * <p>
     * After the second call to <code>compose</code>, therefore, you have what you want:
     * </p>
     *
     * <pre class="stREPL">
     * scala&gt; "9" should beAsIntGreaterThanEight
     *
     * scala&gt; "7" should beAsIntGreaterThanEight
     * org.scalatest.exceptions.TestFailedException: 7 was not greater than 8
     * </pre>
     *
     * <p>
     * So in summary, what the result of <code>(beGreaterThan</code> <code>compose</code> <code>stringToInt)</code> <code>andThen</code>
     * <code>(_</code> <code>compose</code> <code>stringToInt)</code> will do once it is applied to a (right-hand-side)
     * <code>String</code>, is:
     * </p>
     * 
     * <ol>
     * <li>Transform <code>beGreaterThan</code> from an <code>Int</code> <code>=&gt;</code> <code>Matcher[Int]</code>
     * to a <code>String</code> <code>=&gt;</code> <code>Matcher[Int]</code> with the first <code>compose</code></li>
     * <li>Apply the given (right-hand-side) <code>String</code> to that to get a <code>Matcher[Int]</code> (the first part
     * of <code>andThen</code>'s behavior)</li>
     * <li>Pass the resulting <code>Matcher[Int]</code> to the given function,  <code>_</code> <code>compose</code>
     * <code>stringToInt</code>, which will transform the <code>Matcher[Int]</code> to a <code>Matcher[String]</code> (the
     * second part of the <code>andThen</code> behavior).</li>
     * </ol>
     */
    def composeTwice[U](g: U => T): U => Matcher[U] = (f compose g) andThen (_ compose g)

    /**
     * Returns a function that given a <code>T</code> will return a <code>Matcher[T]</code> that will produce the
     * <code>MatchResult</code> produced by <code>f</code> (passed to the <code>Composifier</code> constructor)
     * transformed by the given <code>prettify</code> function.
     *
     * @param prettify a function with which to transform <code>MatchResult</code>s.
     * @return a new <code>Matcher</code> producer function that produces prettified error messages
     */
    def mapResult(prettify: MatchResult => MatchResult): T => Matcher[T] =
      (o: T) => f(o) mapResult prettify

    /**
     * Returns a function that given a <code>T</code> will return a <code>Matcher[T]</code> that will produce the
     * <code>MatchResult</code> produced by <code>f</code> (passed to the <code>Composifier</code> constructor)
     * with arguments transformed by the given <code>prettify</code> function.
     *
     * @param prettify a function with which to transform the arguments of error messages.
     * @return a new <code>Matcher</code> producer function that produces prettified error messages
     */
    def mapArgs(prettify: Any => String): T => Matcher[T] =
      (o: T) => f(o) mapArgs prettify
  }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that converts a function of <code>T =&gt; Matcher[T]</code> to an object that has
   * <code>composeTwice</code>, <code>mapResult</code> and <code>mapArgs</code> methods.
   *
   * The following shows how this trait is used to compose twice and modify error messages:
   *
   * <pre class="stHighlight">
   * import org.scalatest._
   * import matchers._
   * import MatcherProducers._
   *
   * val f = be &gt; (_: Int)
   * val g = (_: String).toInt
   *
   * // f composeTwice g means: (f compose g) andThen (_ compose g)
   * val beAsIntsGreaterThan =
   *   f composeTwice g mapResult { mr =&gt;
   *     mr.copy(
   *       failureMessageArgs =
   *         mr.failureMessageArgs.map((LazyArg(_) { "\"" + _.toString + "\".toInt"})),
   *       negatedFailureMessageArgs =
   *         mr.negatedFailureMessageArgs.map((LazyArg(_) { "\"" + _.toString + "\".toInt"})),
   *       midSentenceFailureMessageArgs =
   *         mr.midSentenceFailureMessageArgs.map((LazyArg(_) { "\"" + _.toString + "\".toInt"})),
   *       midSentenceNegatedFailureMessageArgs =
   *         mr.midSentenceNegatedFailureMessageArgs.map((LazyArg(_) { "\"" + _.toString + "\".toInt"}))
   *     )
   *   }
   *
   * "7" should beAsIntsGreaterThan ("8")
   * </pre>
   *
   * The last assertion will fail with message like this:
   *
   * <pre class="stHighlight">
   * "7".toInt was not greater than "8".toInt
   * </pre>
   *
   * @param f a function that takes a <code>T</code> and return a <code>Matcher[T]</code>
   * @tparam T the type used by function <code>f</code>
   * @return an object that has <code>composeTwice</code>, <code>mapResult</code> and <code>mapArgs</code> methods.
   */
  implicit def convertToComposifier[T](f: T => Matcher[T]): Composifier[T] = new Composifier(f) 
}

/**
 * Companion object that facilitates the importing of <code>MatcherProducers</code> members as
 * an alternative to mixing it in. One use case is to import <code>MatcherProducers</code>'s members so you can use
 * <code>MatcherProducers</code> in the Scala interpreter.
 */
object MatcherProducers extends MatcherProducers

