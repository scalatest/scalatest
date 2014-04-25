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

/**
 * Represents the result of a <em>validation</em>, either the object <a href="Pass$.html"><code>Pass</code></a> if the validation 
 * succeeded, else an instance of <a href="Fail.html"><code>Fail</code></a> containing an error value describing the validation failure.
 *
 * <p>
 * <code>Validation</code>s are used to filter <a href="Or.html"><code>Or</code></a>s in <code>for</code> expressions or <code>filter</code> method calls.
 * For example, consider these methods:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalactic._
 *
 * def isRound(i: Int): Validation[ErrorMessage] =
 *   if (i % 10 == 0) Pass else Fail(i + " was not a round number")
 *
 * def isDivBy3(i: Int): Validation[ErrorMessage] =
 *   if (i % 3 == 0) Pass else Fail(i + " was not divisible by 3")
 * </pre>
 *
 * <p>
 * Because <code>isRound</code> and <code>isDivBy3</code> take an <code>Int</code> and return a <code>Validation[ErrorMessage]</code>, you
 * can use them in filters in <code>for</code> expressions involving <code>Or</code>s of type <code>Int</code> <code>Or</code> <code>ErrorMessage</code>.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * for (i &lt;- Good(3) if isRound(i) &amp;&amp; isDivBy3(i)) yield i
 * // Result: Bad(3 was not a round number)
 * </pre>
 *
 * <p>
 * <code>Validation</code>s can also be used to accumulate error using <code>when</code>, a method that's made available by trait <code>Accumulation</code> on
 * accumualting <code>Or</code>s (<code>Or</code>s whose <code>Bad</code> type is an <code>Every[T]</code>). Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * import Accumulation._
 *
 * for (i &lt;- Good(3) when (isRound, isDivBy3)) yield i
 * // Result: Bad(One(3 was not a round number))
 *
 * for (i &lt;- Good(4) when (isRound, isDivBy3)) yield i
 * // Result: Bad(Many(4 was not a round number, 4 was not divisible by 3))
 * </pre>
 *
 * <p>
 * Note: You can think of <code>Validation</code> as an &ldquo;<code>Option</code> with attitude,&rdquo; where <code>Pass</code> is 
 * a <code>None</code> that indicates validation success and <code>Fail</code> is a <code>Some</code> whose value describes 
 * the validation failure.
 * </p>
 * 
 * @tparam E the type of error value describing a validation failure for this <code>Validation</code>
 */
sealed trait Validation[+E] {

  /**
   * Ands this <code>Validation</code> with another, passed, <code>Validation</code>.
   *
   * <p>
   * The result of and-ing two <code>Validations</code> is:
   * </p>
   *
   * <table style="border-collapse: collapse; border: 1px solid black">
   * <tr><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Expression</th><th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; text-align: center; border: 1px solid black">Result</th></tr><tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Pass &amp;&amp; Pass</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Pass</code></td></tr>
   * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Pass &amp;&amp; Fail(right)</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Fail(right)</code></td></tr>
   * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Fail(left) &amp;&amp; Pass</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Fail(left)</code></td></tr>
   * <tr><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Fail(left) &amp;&amp; Fail(right)</code></td><td style="border-width: 1px; padding: 3px; border: 1px solid black; text-align: center"><code>Fail(left)</code></td></tr>
   * </table>
   *
   * <p>
   * As you can see in the above table, no attempt is made by <code>&amp;&amp;</code> to accumulate errors, which in turn means that
   * no constraint is placed on the <code>E</code> type (it need not be an <code>Every</code>). Instead, <code>&amp;&amp;</code> short circuits
   * and returns the first <code>Fail</code> it encounters. This makes it useful in filters in <code>for</code> expressions involving <a href="Or.html"><code>Or</code></a>s.
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalactic._
   *
   * def isRound(i: Int): Validation[ErrorMessage] =
   *   if (i % 10 != 0) Fail(i + " was not a round number") else Pass
   *
   * def isDivBy3(i: Int): Validation[ErrorMessage] =
   *   if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass
   *
   * for (i &lt;- Good(3) if isRound(i) &amp;&amp; isDivBy3(i)) yield i
   * // Result: Bad(3 was not a round number)
   * </pre>
   *
   * @param other the other validation to and with this one
   * @return the result of anding this <code>Validation</code> with the other, passed, <code>Validation</code>
   */
  def &&[F >: E](other: => Validation[F]): Validation[F]
}

/**
 * Indicates a validation succeeded.
 */
case object Pass extends Validation[Nothing] {

  /**
   * Ands this <code>Validation</code> with another, passed, <code>Validation</code>.
   *
   * <p>
   * The result of invoking this method will be the result of executing the passed <code>other</code> by-name value.
   * </p>
   *
   * @param other the other validation to and with this one
   * @return the result of anding this <code>Validation</code> with the other, passed, <code>Validation</code>
   */
  def &&[F](other: => Validation[F]): Validation[F] = other
}

/**
 * Indicates a validation failed, describing the failure with a contained error value.
 *
 * @param error an error value describing the validation failure
 * @tparam E the type of value describing a validation failure for this <code>Fail</code>
 */
case class Fail[E](error: E) extends Validation[E] {

  /**
   * Ands this <code>Validation</code> with another, passed, <code>Validation</code>.
   *
   * <p>
   * The result of invoking this method will be this same <code>Fail</code> object. It will not execute the passed by-name.
   * </p>
   *
   * @param other the other validation to and with this one
   * @return the result of anding this <code>Validation</code> with the other, passed, <code>Validation</code>
   */
  def &&[F >: E](other: => Validation[F]): Validation[F] = this
}

/*
OK, I can keep Validation simple like this, and it will only support &&, which
will short-circuit like && does, at least in the error message. And when I do Expectation, maybe I
could put it in Scalactic, and have an implicit conversion in the Expectation or Validation
companion object that goes from Expectation to Validation *if* the error type is String.
Though not sure that's really a good way to create error messages for non-programmers.
*/
