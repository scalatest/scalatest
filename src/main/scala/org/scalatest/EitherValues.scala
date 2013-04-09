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
 * Trait that provides an implicit conversion that adds <code>left.value</code> and <code>right.value</code> methods
 * to <code>Either</code>, which will return the selected value of the <code>Either</code> if defined,
 * or throw <code>TestFailedException</code> if not.
 *
 * <p>
 * This construct allows you to express in one statement that an <code>Either</code> should be <em>left</em> or <em>right</em>
 * and that its value should meet some expectation. Here's are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * either1.right.value should be &gt; 9
 * either2.left.value should be ("Muchas problemas")
 * </pre>
 *
 * <p>
 * Or, using assertions instead of matcher expressions:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(either1.right.value &gt; 9)
 * assert(either2.left.value === "Muchas problemas")
 * </pre>
 *
 * <p>
 * Were you to simply invoke <code>right.get</code> or <code>left.get</code> on the <code>Either</code>, 
 * if the <code>Either</code> wasn't defined as expected (<em>e.g.</em>, it was a <code>Left</code> when you expected a <code>Right</code>), it
 * would throw a <code>NoSuchElementException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Left("Muchas problemas")
 *
 * either.right.get should be &gt; 9 // either.right.get throws NoSuchElementException
 * </pre>
 *
 * <p>
 * The <code>NoSuchElementException</code> would cause the test to fail, but without providing a <a href="StackDepth.html">stack depth</a> pointing
 * to the failing line of test code. This stack depth, provided by <a href="TestFailedException.html"><code>TestFailedException</code></a> (and a
 * few other ScalaTest exceptions), makes it quicker for
 * users to navigate to the cause of the failure. Without <code>EitherValues</code>, to get
 * a stack depth exception you would need to make two statements, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Left("Muchas problemas")
 *
 * either should be ('right) // throws TestFailedException
 * either.right.get should be &gt; 9
 * </pre>
 *
 * <p>
 * The <code>EitherValues</code> trait allows you to state that more concisely:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Left("Muchas problemas")
 *
 * either.right.value should be &gt; 9 // either.right.value throws TestFailedException
 * </pre>
 */
trait EitherValues {

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>LeftProjection</code>.
   *
   * @param either the <code>LeftProjection</code> on which to add the <code>value</code> method
   */
  implicit def convertLeftProjectionToValuable[L, R](leftProj: Either.LeftProjection[L, R]) = new LeftValuable(leftProj)

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>RightProjection</code>.
   *
   * @param either the <code>RightProjection</code> on which to add the <code>value</code> method
   */
  implicit def convertRightProjectionToValuable[L, R](rightProj: Either.RightProjection[L, R]) = new RightValuable(rightProj)

  /**
   * Wrapper class that adds a <code>value</code> method to <code>LeftProjection</code>, allowing
   * you to make statements like:
   *
   * <pre class="stHighlight">
   * either.left.value should be &gt; 9
   * </pre>
   *
   * @param leftProj A <code>LeftProjection</code> to convert to <code>LeftValuable</code>, which provides the
   *   <code>value</code> method.
   */
  class LeftValuable[L, R](leftProj: Either.LeftProjection[L, R]) {

    /**
     * Returns the <code>Left</code> value contained in the wrapped <code>LeftProjection</code>, if defined as a <code>Left</code>, else throws <code>TestFailedException</code> with
     * a detail message indicating the <code>Either</code> was defined as a <code>Right</code>, not a <code>Left</code>.
     */
    def value: L = {
      try {
        leftProj.get
      }
      catch {
        case cause: NoSuchElementException => 
          throw new TestFailedException(sde => Some(Resources("eitherLeftValueNotDefined")), Some(cause), getStackDepthFun("EitherValues.scala", "value"))
      }
    }
  }

  /**
   * Wrapper class that adds a <code>value</code> method to <code>RightProjection</code>, allowing
   * you to make statements like:
   *
   * <pre class="stHighlight">
   * either.right.value should be &gt; 9
   * </pre>
   *
   * @param rightProj A <code>RightProjection</code> to convert to <code>RightValuable</code>, which provides the
   *   <code>value</code> method.
   */
  class RightValuable[L, R](rightProj: Either.RightProjection[L, R]) {

    /**
     * Returns the <code>Right</code> value contained in the wrapped <code>RightProjection</code>, if defined as a <code>Right</code>, else throws <code>TestFailedException</code> with
     * a detail message indicating the <code>Either</code> was defined as a <code>Right</code>, not a <code>Left</code>.
     */
    def value: R = {
      try {
        rightProj.get
      }
      catch {
        case cause: NoSuchElementException => 
          throw new TestFailedException(sde => Some(Resources("eitherRightValueNotDefined")), Some(cause), getStackDepthFun("EitherValues.scala", "value"))
      }
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>ValueEither</code> members as 
 * an alternative to mixing it in. One use case is to import <code>EitherValues</code>'s members so you can use
 * <code>left.value</code> and <code>right.value</code> on <code>Either</code> in the Scala interpreter:
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
 * scala&gt; import EitherValues._
 * import EitherValues._
 * 
 * scala&gt; val e: Either[String, Int] = Left("Muchas problemas")
 * e: Either[String,Int] = Left(Muchas problemas)
 * 
 * scala&gt; e.left.value should be ("Muchas problemas")
 * 
 * scala&gt; e.right.value should be &lt; 9
 * org.scalatest.TestFailedException: The Either on which rightValue was invoked was not defined.
 *   at org.scalatest.EitherValues$RightValuable.value(EitherValues.scala:148)
 *   at .&lt;init&gt;(&lt;console&gt;:18)
 *   ...
 * </pre>
 */
object EitherValues extends EitherValues
