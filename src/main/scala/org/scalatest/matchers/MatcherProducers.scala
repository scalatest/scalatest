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
 * Trait that provides the ability to modify error messages when composing <code>Matcher</code>s.
 *
 * @author Bill Venners
 * @author Chee Seng
 */
trait MatcherProducers {

  class Composifier[T](f: T => Matcher[T]) {
    def composeTwice[U](g: U => T): U => Matcher[U] = (f compose g) andThen (_ compose g)
    def mapResult(prettify: MatchResult => MatchResult): T => Matcher[T] =
      (o: T) => f(o) mapResult prettify
    def mapArgs(prettify: Any => String): T => Matcher[T] =
      (o: T) => f(o) mapArgs prettify
  }

  /**
   * Implicit conversion that converts a function of <code>T => Matcher[T]</code> to an object that has
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
   * val beAsIntsGreaterThan = f composeTwice g mapResult { mr =>
   *   mr.copy(
   *     failureMessageArgs = mr.failureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
   *     negatedFailureMessageArgs = mr.negatedFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
   *     midSentenceFailureMessageArgs =
   *       mr.midSentenceFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"})),
   *     midSentenceNegatedFailureMessageArgs =
   *       mr.midSentenceNegatedFailureMessageArgs.map((LazyArg(_) { _.toString + ".toInt"}))
   *   )
   * }
   *
   * "7" should beAsIntsGreaterThan ("8")
   *
   * </pre>
   *
   * The last assertion will fail with message like this:
   *
   * <pre class="stHighlight">
   * 7.toInt was not greater than 8.toInt
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

