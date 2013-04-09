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

import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

/**
 * Trait that provides an implicit conversion that adds a <code>valueAt</code> method
 * to <code>PartialFunction</code>, which will return the value (result) of the function applied to the argument passed to <code>valueAt</code>,
 * or throw <code>TestFailedException</code> if the partial function is not defined at the argument.
 *
 * <p>
 * This construct allows you to express in one statement that a partial function should be defined for a particular input,
 * and that its result value should meet some expectation. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * pf.valueAt("IV") should equal (4)
 * </pre>
 *
 * <p>
 * Or, using an assertion instead of a matcher expression:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(pf.valueAt("IV") === 4)
 * </pre>
 *
 * <p>
 * Were you to simply invoke <code>apply</code> on the <code>PartialFunction</code>, passing in an input value, 
 * if the partial function wasn't defined at that input, it would throw some exception, but likely not one
 * that provides a <a href="StackDepth.html">stack depth</a>:
 * </p>
 *
 * <pre class="stHighlight">
 * // Note: a Map[K, V] is a PartialFunction[K, V]
 * val pf: PartialFunction[String, Int] = Map("I" -&gt; 1, "II" -&gt; 2, "III" -&gt; 3, "IV" -&gt; 4)
 *
 * pf("V") should equal (5) // pf("V") throws NoSuchElementException
 * </pre>
 *
 * <p>
 * The <code>NoSuchElementException</code> thrown in this situation would cause the test to fail, but without providing a stack depth pointing
 * to the failing line of test code. This stack depth, provided by <a href="TestFailedException.html"><code>TestFailedException</code></a> (and a
 * few other ScalaTest exceptions), makes it quicker for
 * users to navigate to the cause of the failure. Without <code>PartialFunctionValues</code>, to get
 * a stack depth exception you would need to make two statements, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val pf: PartialFunction[String, Int] = Map("I" -&gt; 1, "II" -&gt; 2, "III" -&gt; 3, "IV" -&gt; 4)
 *
 * pf.isDefinedAt("V") should be (true) // throws TestFailedException
 * pf("V") should equal (5)
 * </pre>
 *
 * <p>
 * The <code>PartialFunctionValues</code> trait allows you to state that more concisely:
 * </p>
 *
 * <pre class="stHighlight">
 * val pf: PartialFunction[String, Int] = Map("I" -&gt; 1, "II" -&gt; 2, "III" -&gt; 3, "IV" -&gt; 4)
 *
 * pf.valueAt("V") should equal (5) // pf.valueAt("V") throws TestFailedException
 * </pre>
 */
trait PartialFunctionValues {

  /**
   * Implicit conversion that adds a <code>valueAt</code> method to <code>PartialFunction</code>.
   *
   * @param pf the <code>PartialFunction</code> on which to add the <code>valueAt</code> method
   */
  implicit def convertPartialFunctionToValuable[A, B](pf: PartialFunction[A, B]) = new Valuable(pf)
  
  /**
   * Wrapper class that adds a <code>valueAt</code> method to <code>PartialFunction</code>, allowing
   * you to make statements like:
   *
   * <pre class="stHighlight">
   * pf.valueAt("VI") should equal (6)
   * </pre>
   *
   * @param pf An <code>PartialFunction</code> to convert to <code>Valuable</code>, which provides the <code>valueAt</code> method.
   */
  class Valuable[A, B](pf: PartialFunction[A, B]) {

    /**
     * Returns the result of applying the wrapped <code>PartialFunction</code> to the passed input, if it is defined at that input, else
     * throws <code>TestFailedException</code> with a detail message indicating the <code>PartialFunction</code> was not defined at the given input.
     */
    def valueAt(input: A): B = {
      if (pf.isDefinedAt(input)) {
        pf.apply(input)
      }
      else
        throw new TestFailedException(sde => Some(Resources("partialFunctionValueNotDefined", input.toString)), None, getStackDepthFun("PartialFunctionValues.scala", "valueAt"))
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>PartialFunctionValues</code> members as 
 * an alternative to mixing it in. One use case is to import <code>PartialFunctionValues</code>'s members so you can use
 * the <code>valueAt</code> method on <code>PartialFunction</code> in the Scala interpreter:
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
 * scala&gt; import PartialFunctionValues._
 * import PartialFunctionValues._
 * 
 * scala&gt; val pf: PartialFunction[String, Int] = Map("I" -&gt; 1, "II" -&gt; 2, "III" -&gt; 3, "IV" -&gt; 4)
 * pf: PartialFunction[String,Int] = Map(I -&gt; 1, II -&gt; 2, III -&gt; 3, IV -&gt; 4)
 * 
 * scala&gt; pf("IV") should equal (4)
 * 
 * scala&gt; pf("V") should equal (5)
 * java.util.NoSuchElementException: key not found: V
 *   at scala.collection.MapLike$class.default(MapLike.scala:224)
 *   at scala.collection.immutable.Map$Map4.default(Map.scala:167)
 *   ...
 * </pre>
 */
object PartialFunctionValues extends PartialFunctionValues
