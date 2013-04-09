/*
 * Copyright 2001-2011 Artima, Inc.
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
package org.scalatest

import java.util.NoSuchElementException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

/**
 * Trait that provides an implicit conversion that adds a <code>value</code> method
 * to <code>Option</code>, which will return the value of the option if it is defined,
 * or throw <code>TestFailedException</code> if not.
 *
 * <p>
 * This construct allows you to express in one statement that an option should be defined
 * and that its value should meet some expectation. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * opt.value should be &gt; 9
 * </pre>
 *
 * <p>
 * Or, using an assertion instead of a matcher expression:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(opt.value &gt; 9)
 * </pre>
 *
 * <p>
 * Were you to simply invoke <code>get</code> on the <code>Option</code>, 
 * if the option wasn't defined, it would throw a <code>NoSuchElementException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val opt: Option[Int] = None
 *
 * opt.get should be &gt; 9 // opt.get throws NoSuchElementException
 * </pre>
 *
 * <p>
 * The <code>NoSuchElementException</code> would cause the test to fail, but without providing a <a href="StackDepth.html">stack depth</a> pointing
 * to the failing line of test code. This stack depth, provided by <a href="TestFailedException.html"><code>TestFailedException</code></a> (and a
 * few other ScalaTest exceptions), makes it quicker for
 * users to navigate to the cause of the failure. Without <code>OptionValues</code>, to get
 * a stack depth exception you would need to make two statements, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val opt: Option[Int] = None
 *
 * opt should be ('defined) // throws TestFailedException
 * opt.get should be &gt; 9
 * </pre>
 *
 * <p>
 * The <code>OptionValues</code> trait allows you to state that more concisely:
 * </p>
 *
 * <pre class="stHighlight">
 * val opt: Option[Int] = None
 *
 * opt.value should be &gt; 9 // opt.value throws TestFailedException
 * </pre>
 */
trait OptionValues {

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>Option</code>.
   *
   * @param opt the <code>Option</code> on which to add the <code>value</code> method
   */
  implicit def convertOptionToValuable[T](opt: Option[T]) = new Valuable(opt)

  /**
   * Wrapper class that adds a <code>value</code> method to <code>Option</code>, allowing
   * you to make statements like:
   *
   * <pre class="stHighlight">
   * opt.value should be &gt; 9
   * </pre>
   *
   * @param opt An option to convert to <code>Valuable</code>, which provides the <code>value</code> method.
   */
  class Valuable[T](opt: Option[T]) {

    /**
     * Returns the value contained in the wrapped <code>Option</code>, if defined, else throws <code>TestFailedException</code> with
     * a detail message indicating the option was not defined.
     */
    def value: T = {
      try {
        opt.get
      }
      catch {
        case cause: NoSuchElementException => 
          throw new TestFailedException(sde => Some(Resources("optionValueNotDefined")), Some(cause), getStackDepthFun("OptionValues.scala", "value"))
      }
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>OptionValues</code> members as 
 * an alternative to mixing it in. One use case is to import <code>OptionValues</code>'s members so you can use
 * <code>value</code> on option in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -cp scalatest-1.7.jar
 * Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import matchers.ShouldMatchers._
 * import matchers.ShouldMatchers._
 *
 * scala&gt; import OptionValues._
 * import OptionValues._
 *
 * scala&gt; val opt1: Option[Int] = Some(1)
 * opt1: Option[Int] = Some(1)
 * 
 * scala&gt; val opt2: Option[Int] = None
 * opt2: Option[Int] = None
 * 
 * scala&gt; opt1.value should be &lt; 10
 * 
 * scala&gt; opt2.value should be &lt; 10
 * org.scalatest.TestFailedException: The Option on which value was invoked was not defined.
 *   at org.scalatest.OptionValues$Valuable.value(OptionValues.scala:68)
 *   at .&lt;init&gt;(&lt;console&gt;:18)
 *   ...
 * </pre>
 *
 */
object OptionValues extends OptionValues

