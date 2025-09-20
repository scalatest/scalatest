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
package org.scalatest.verbs

import org.scalactic._

/**
 * Provides an implicit conversion that adds <code>can</code> methods to <code>String</code>
 * to support the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>org.scalatest.fixture.FlatSpec</code>,
 * and <code>fixture.WordSpec</code>.
 *
 * <p>
 * For example, this trait enables syntax such as the following test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" can "be empty" in { ... }
 *                        ^
 * </pre>
 *
 * <p>
 * It also enables syntax such as the following shared test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (with one item)" can behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
 *                           ^
 * </pre>
 *
 * <p>
 * In addition, it supports the registration of subject descriptions in <code>WordSpec</code>
 * and <code>fixture.WordSpec</code>, such as:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" can { ...
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
 *   "The ScalaTest Matchers DSL" can provide {
 *                                ^
 * </pre>
 *
 * <p>
 * The reason this implicit conversion is provided in a separate trait, instead of being provided
 * directly in <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>, and
 * <code>fixture.WordSpec</code>, is primarily for design symmetry with <code>ShouldVerb</code>
 * and <code>MustVerb</code>. Both <code>ShouldVerb</code> and <code>MustVerb</code> must exist
 * as a separate trait because an implicit conversion provided directly would conflict
 * with the implicit conversion that provides <code>should</code> or <code>must</code> methods on <code>String</code>
 * in the <code>Matchers</code> and <code>MustMatchers</code> traits.
 * </p>
 *
 * @author Bill Venners
 */
trait CanVerb {

  import CanVerb.StringCanWrapperForVerb
  // SKIP-DOTTY-START 
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
  // SKIP-DOTTY-START
   * Implicitly converts an object of type <code>String</code> to a <code>StringCanWrapper</code>,
  // SKIP-DOTTY-END
  //DOTTY-ONLY   * Converts an object of type <code>String</code> to a <code>StringCanWrapper</code>,
   * to enable <code>can</code> methods to be invokable on that object.
   */
  // SKIP-DOTTY-START 
  implicit def convertToStringCanWrapper(o: String)(implicit position: source.Position): StringCanWrapperForVerb =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertToStringCanWrapper(o: String)(using position: source.Position): StringCanWrapperForVerb =
    new StringCanWrapperForVerb {
      val leftSideString = o.trim
      val pos = position
    }
  //DOTTY-ONLY extension (o: String)(using position: source.Position) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Extension method to support test registration in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For example, this method enables syntax such as the following in <code>FlatSpec</code>
  //DOTTY-ONLY    * and <code>fixture.FlatSpec</code>:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack (when empty)" can "be empty" in { ... }
  //DOTTY-ONLY    *                        ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
  //DOTTY-ONLY    * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
  //DOTTY-ONLY    * simply invokes this function, passing in leftSideString, the verb string
  //DOTTY-ONLY    * <code>"can"</code>, and right, and returns the result.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def can(right: String)(using svsi: StringVerbStringInvocation): ResultOfStringPassedToVerb = convertToStringCanWrapper(o).can(right)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Extension method to support shared test registration in <code>FlatSpec</code>
  //DOTTY-ONLY    * and <code>fixture.FlatSpec</code>.
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For example, this method enables syntax such as the following in <code>FlatSpec</code>
  //DOTTY-ONLY    * and <code>fixture.FlatSpec</code>:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack (with one item)" can behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
  //DOTTY-ONLY    *                           ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> passes in a function via the implicit parameter that takes
  //DOTTY-ONLY    * a string and results in a <code>BehaveWord</code>. This method
  //DOTTY-ONLY    * simply invokes this function, passing in leftSideString, and returns the result.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def can(right: BehaveWord)(using svbli: StringVerbBehaveLikeInvocation): BehaveWord = convertToStringCanWrapper(o).can(right)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Extension method to support the registration of subject descriptions in <code>WordSpec</code>
  //DOTTY-ONLY    * and <code>fixture.WordSpec</code>.
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For example, this method enables syntax such as the following in <code>WordSpec</code>
  //DOTTY-ONLY    * and <code>fixture.WordSpec</code>:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack (when empty)" can { ...
  //DOTTY-ONLY    *                        ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * <code>WordSpec</code> passes in a function via the implicit parameter of type <code>StringVerbBlockRegistration</code>,
  //DOTTY-ONLY    * a function that takes two strings and a no-arg function and results in <code>Unit</code>. This method
  //DOTTY-ONLY    * simply invokes this function, passing in leftSideString, the verb string
  //DOTTY-ONLY    * <code>"can"</code>, and the right by-name parameter transformed into a
  //DOTTY-ONLY    * no-arg function.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def can(right: => Unit)(using fun: StringVerbBlockRegistration): Unit = convertToStringCanWrapper(o).can(right)
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Extension method to support the registration of subject descriptions with after words
  //DOTTY-ONLY    * in <code>WordSpec</code> and <code>fixture.WordSpec</code>.
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For example, this method enables syntax such as the following in <code>WordSpec</code>
  //DOTTY-ONLY    * and <code>fixture.WordSpec</code>:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    *    def provide = afterWord("provide")
  //DOTTY-ONLY    *
  //DOTTY-ONLY    *   "The ScalaTest Matchers DSL" can provide {
  //DOTTY-ONLY    *                                ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * <code>WordSpec</code> passes in a function via the implicit parameter that takes
  //DOTTY-ONLY    * two strings and a <code>ResultOfAfterWordApplication</code> and results in <code>Unit</code>. This method
  //DOTTY-ONLY    * simply invokes this function, passing in leftSideString, the verb string
  //DOTTY-ONLY    * <code>"can"</code>, and the <code>ResultOfAfterWordApplication</code> passed to <code>can</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   def can(resultOfAfterWordApplication: ResultOfAfterWordApplication)(using swawr: SubjectWithAfterWordRegistration): Unit = convertToStringCanWrapper(o).can(resultOfAfterWordApplication)
  //DOTTY-ONLY }
}

/**
 * Companion object for the <code>CanVerb</code> trait.
 */
object CanVerb extends CanVerb {

  /**
   * This class supports the syntax of <code>FlatSpec</code>, <code>WordSpec</code>, <code>fixture.FlatSpec</code>,
   * and <code>fixture.WordSpec</code>.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>can</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  trait StringCanWrapperForVerb {

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
     * "A Stack (when empty)" can "be empty" in { ... }
     *                        ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> passes in a function via the implicit parameter that takes
     * three strings and results in a <code>ResultOfStringPassedToVerb</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"can"</code>, and right, and returns the result.
     * </p>
     */
    //DOTTY-ONLY infix 
    def can(right: String)(implicit svsi: StringVerbStringInvocation): ResultOfStringPassedToVerb = {
      svsi(leftSideString, "can", right, pos)
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
     * "A Stack (with one item)" can behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
     *                           ^
     * </pre>
     *
     * <p>
     * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> passes in a function via the implicit parameter that takes
     * a string and results in a <code>BehaveWord</code>. This method
     * simply invokes this function, passing in leftSideString, and returns the result.
     * </p>
     */
    //DOTTY-ONLY infix 
    def can(right: BehaveWord)(implicit svbli: StringVerbBehaveLikeInvocation): BehaveWord = {
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
     * "A Stack (when empty)" can { ...
     *                        ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter of type <code>StringVerbBlockRegistration</code>,
     * a function that takes two strings and a no-arg function and results in <code>Unit</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"can"</code>, and the right by-name parameter transformed into a
     * no-arg function.
     * </p>
     */
    //DOTTY-ONLY infix 
    def can(right: => Unit)(implicit fun: StringVerbBlockRegistration): Unit = {
      fun(leftSideString, "can", pos, () => right)
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
     *   "The ScalaTest Matchers DSL" can provide {
     *                                ^
     * </pre>
     *
     * <p>
     * <code>WordSpec</code> passes in a function via the implicit parameter that takes
     * two strings and a <code>ResultOfAfterWordApplication</code> and results in <code>Unit</code>. This method
     * simply invokes this function, passing in leftSideString, the verb string
     * <code>"can"</code>, and the <code>ResultOfAfterWordApplication</code> passed to <code>can</code>.
     * </p>
     */
    //DOTTY-ONLY infix 
    def can(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit swawr: SubjectWithAfterWordRegistration): Unit = {
      swawr(leftSideString, "can", resultOfAfterWordApplication, pos)
    }
  }

}
