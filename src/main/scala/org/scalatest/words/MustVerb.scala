/*
 * Copyright 2001-2009 Artima, Inc.
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

import org.scalatest._

/**
 * Provides an implicit conversion that adds <code>must</code> methods to <code>String</code>
 * to support the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>,
 * and <code>fixture.WordSpec</code>.
 *
 * <p>
 * For example, this trait enables syntax such as the following test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" must "be empty" in { ... }
 *                        ^
 * </pre>
 *
 * <p>
 * It also enables syntax such as the following shared test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (with one item)" must behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *                           ^
 * </pre>
 *
 * <p>
 * In addition, it supports the registration of subject descriptions in <code>WordSpec</code>
 * and <code>fixture.WordSpec</code>, such as:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" must { ...
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
 *   "The ScalaTest Matchers DSL" must provide {
 *                                ^
 * </pre>
 *
 * <p>
 * The reason this implicit conversion is provided in a separate trait, instead of being provided
 * directly in <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, and
 * <code>fixture.WordSpec</code>, is because an implicit conversion provided directly would conflict
 * with the implicit conversion that provides <code>must</code> methods on <code>String</code>
 * in the <code>MustMatchers</code> trait. By contrast, there is no conflict with
 * the separate <code>MustVerb</code> trait approach, because:
 * </p>
 *
 * <ol>
 * <li><code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, and <code>fixture.WordSpec</code>
 * mix in <code>MustVerb</code> directly, and</li>
 * <li><code>MustMatchers</code> extends <code>MustVerb</code>, overriding the
 * <code>convertToStringMustWrapper</code> implicit conversion function.</li>
 * </ol>
 *
 * <p>
 * So whether or not
 * a <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, or <code>fixture.WordSpec</code>
 * mixes in <code>MustMatchers</code>, there will only be one
 * implicit conversion in scope that adds <code>must</code> methods to <code>String</code>s.
 * </p>
 *
 * </p>
 * Also, because the class of the result of the overriding <code>convertToStringMustWrapper</code>
 * implicit conversion method provided in <code>MustMatchers</code> extends this trait's
 * <code>StringMustWrapperForVerb</code> class, the four uses of <code>must</code> provided here
 * are still available. These four <code>must</code> are in fact available to any class
 * that mixes in <code>MustMatchers</code>, but each takes an implicit parameter that is provided
 * only in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>, or <code>WordSpec</code> and
 * <code>fixture.WordSpec</code>.  
 * </p>
 *
 * @author Bill Venners
 */
trait MustVerb {

  /**
   * This class supports the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>,
   * and <code>fixture.WordSpec</code>.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>must</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class StringMustWrapperForVerb(left: String) {

    /**
     * Supports test registration in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
     *
     * <p>
     * For example, this method enables syntax such as the following in <code>FlatSpec</code>
     * and <code>fixture.FlatSpec</code>:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack (when empty)" must "be empty" in { ... }
     *                        ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
     * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
     * simply invokes this function, passing in left, the verb string
     * <code>"must"</code>, and right, and returns the result.
     * </p>
     */
    def must(right: String)(implicit fun: (String, String, String) => ResultOfStringPassedToVerb): ResultOfStringPassedToVerb = {
      fun(left, "must", right)
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
     * "A Stack (with one item)" must behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
     *                           ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> passes in a function via the implicit parameter that takes
     * a string and results in a <code>BehaveWord</code>. This method
     * simply invokes this function, passing in left, and returns the result.
     * </p>
     */
    def must(right: BehaveWord)(implicit fun: (String) => BehaveWord): BehaveWord = {
      fun(left)
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
     * "A Stack (when empty)" must { ...
     *                        ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter of type <code>StringVerbBlockRegistration</code>,
     * a function that takes two strings and a no-arg function and results in <code>Unit</code>. This method
     * simply invokes this function, passing in left, the verb string
     * <code>"must"</code>, and the right by-name parameter transformed into a
     * no-arg function.
     * </p>
     */
    def must(right: => Unit)(implicit fun: StringVerbBlockRegistration) {
      fun(left, "must", right _)
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
     *   "The ScalaTest Matchers DSL" must provide {
     *                                ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter that takes
     * two strings and a <code>ResultOfAfterWordApplication</code> and results in <code>Unit</code>. This method
     * simply invokes this function, passing in left, the verb string
     * <code>"must"</code>, and the <code>ResultOfAfterWordApplication</code> passed to <code>must</code>.
     * </p>
     */
    def must(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit fun: (String, String, ResultOfAfterWordApplication) => Unit) {
      fun(left, "must", resultOfAfterWordApplication)
    }
  }

  /**
   * Implicitly converts an object of type <code>String</code> to a <code>StringMustWrapper</code>,
   * to enable <code>must</code> methods to be invokable on that object.
   */
  implicit def convertToStringMustWrapper(o: String): StringMustWrapperForVerb = new StringMustWrapperForVerb(o.trim)
}
