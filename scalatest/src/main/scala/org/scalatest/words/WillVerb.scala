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

import org.scalactic._

/**
 * Provides an implicit conversion that adds <code>will</code> methods to <code>String</code>
 * to support the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>,
 * and <code>fixture.WordSpec</code>.
 *
 * <p>
 * For example, this trait enables syntax such as the following test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" will "be empty" in { ... }
 *                        ^
 * </pre>
 *
 * <p>
 * It also enables syntax such as the following shared test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (with one item)" will behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *                           ^
 * </pre>
 *
 * <p>
 * In addition, it supports the registration of subject descriptions in <code>WordSpec</code>
 * and <code>fixture.WordSpec</code>, such as:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" will { ...
 *                        ^
 * </pre>
 *
 * <p>
 * And finally, it also supportds the registration of subject descriptions with after words
 * in <code>WordSpec</code> and <code>fixture.WordSpec</code>. For example:
 * </p>
 *
 * <pre class="stHighlight">
 *    def provide = afterWord("provide")
 *
 *   "The ScalaTest Matchers DSL" will provide {
 *                                ^
 * </pre>
 *
 * <p>
 * The reason this implicit conversion is provided in a separate trait, instead of being provided
 * directly in <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, and
 * <code>fixture.WordSpec</code>, is because an implicit conversion provided directly would conflict
 * with the implicit conversion that provides <code>will</code> methods on <code>String</code>
 * in the <code>Matchers</code> trait. By contrast, there is no conflict with
 * the separate <code>WillVerb</code> trait approach, because:
 * </p>
 *
 * <ol>
 * <li><code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, and <code>fixture.WordSpec</code>
 * mix in <code>WillVerb</code> directly, and</li>
 * <li><code>Matchers</code> extends <code>WillVerb</code>, overriding the
 * <code>convertToStringWillWrapper</code> implicit conversion function.</li>
 * </ol>
 *
 * <p>
 * So whether or not
 * a <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, or <code>fixture.WordSpec</code>
 * mixes in <code>Matchers</code>, there will only be one
 * implicit conversion in scope that adds <code>will</code> methods to <code>String</code>s.
 * </p>
 *
 * </p>
 * Also, because the class of the result of the overriding <code>convertToStringWillWrapper</code>
 * implicit conversion method provided in <code>Matchers</code> extends this trait's
 * <code>StringWillWrapperForVerb</code> class, the four uses of <code>will</code> provided here
 * are still available. These four <code>will</code> are in fact available to any class
 * that mixes in <code>Matchers</code>, but each takes an implicit parameter that is provided
 * only in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>, or <code>WordSpec</code> and
 * <code>fixture.WordSpec</code>.
 * </p>
 *
 * @author Bill Venners
 */
private[scalatest] trait WillVerb {

  // This can't be final or abstract, because it is instantiated directly by the implicit conversion, and
  // extended by something in Matchers.
  /**
   * This class supports the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>,
   * and <code>fixture.WordSpec</code>.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>will</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  trait StringWillWrapperForVerb {

    // Don't use "left" because that conflicts with Scalaz's left method on strings
    val leftSideString: String

    val pos: source.Position

    /**
     * Supports test registration in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
     *
     * <p>
     * For example, this method enables syntax such as the following in <code>FlatSpec</code>
     * and <code>fixture.FlatSpec</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack (when empty)" will "be empty" in { ... }
     *                        ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
     * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"will"</code>, and right, and returns the result.
     * </p>
     */
    def will(right: String)(implicit svsi: StringVerbStringInvocation): ResultOfStringPassedToVerb = {
      svsi(leftSideString, "will", right, pos)
    }

    /**
     * Supports shared test registration in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
     *
     * <p>
     * For example, this method enables syntax such as the following in <code>FlatSpec</code>
     * and <code>fixture.FlatSpec</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack (with one item)" will behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
     *                           ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> passes in a function via the implicit parameter that takes
     * a string and results in a <code>BehaveWord</code>. This method
     * simply invokes this function, passing in leftSideString, and returns the result.
     * </p>
     */
    def will(right: BehaveWord)(implicit svbli: StringVerbBehaveLikeInvocation): BehaveWord = {
      svbli(leftSideString, pos)
    }

    /**
     * Supports the registration of subject descriptions in <code>WordSpec</code>
     * and <code>fixture.WordSpec</code>.
     *
     * <p>
     * For example, this method enables syntax such as the following in <code>WordSpec</code>
     * and <code>fixture.WordSpec</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack (when empty)" will { ...
     *                        ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter of type <code>StringVerbBlockRegistration</code>,
     * a function that takes two strings and a no-arg function and results in <code>Unit</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"will"</code>, and the right by-name parameter transformed into a
     * no-arg function.
     * </p>
     */
    def will(right: => Unit)(implicit fun: StringVerbBlockRegistration): Unit = {
      fun(leftSideString, "will", pos, () => right)
    }

    /**
     * Supports the registration of subject descriptions with after words
     * in <code>WordSpec</code> and <code>fixture.WordSpec</code>.
     *
     * <p>
     * For example, this method enables syntax such as the following in <code>WordSpec</code>
     * and <code>fixture.WordSpec</code>:
     * </p>
     *
     * <pre class="stHighlight">
     *    def provide = afterWord("provide")
     *
     *   "The ScalaTest Matchers DSL" will provide {
     *                                ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter that takes
     * two strings and a <code>ResultOfAfterWordApplication</code> and results in <code>Unit</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"will"</code>, and the <code>ResultOfAfterWordApplication</code> passed to <code>will</code>.
     * </p>
     */
    def will(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit swawr: SubjectWithAfterWordRegistration): Unit = {
      swawr(leftSideString, "will", resultOfAfterWordApplication, pos)
    }
  }

  import scala.language.implicitConversions

  /**
   * Implicitly converts an object of type <code>String</code> to a <code>StringWillWrapperForVerb</code>,
   * to enable <code>will</code> methods to be invokable on that object.
   */
  implicit def convertToStringWillWrapperForVerb(o: String)(implicit position: source.Position): StringWillWrapperForVerb =
    new StringWillWrapperForVerb {
      val leftSideString = o.trim
      val pos = position
    }
}

