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
package org.scalatest

import org.scalactic.{Resources => _, _}
import org.scalatest.exceptions.StackDepthException
import org.scalatest.exceptions.TestFailedException

/**
 * Trait that provides an implicit conversion that adds <code>value</code> (when you expect a Right)
 * and <code>left.value</code> (when you expect a Left) methods
 * to <code>Either</code>, which will return the selected value of the <code>Either</code> if defined,
 * or throw <code>TestFailedException</code> if not.
 *
 * <p>
 * This construct allows you to express in one statement that an <code>Either</code> should be <em>left</em> or <em>right</em>
 * and that its value should meet some expectation. Here's are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * either1.value should be &gt; 9
 * either2.left.value should be ("Muchos problemas")
 * </pre>
 *
 * <p>
 * Or, using assertions instead of matcher expressions:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(either1.value &gt; 9)
 * assert(either2.left.value === "Muchos problemas")
 * </pre>
 *
 * <p>
 * Were you to simply invoke <code>left.get</code> on the <code>Either</code>,
 * if the <code>Either</code> wasn't defined as expected (<em>e.g.</em>, it was a <code>Right</code> when you expected a <code>Left</code>), it
 * would throw a <code>NoSuchElementException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Right(9)
 *
 * either.left.get should be &gt; "Muchos problemas" // either.right.get throws NoSuchElementException
 * </pre>
 *
 * <p>
 * The <code>NoSuchElementException</code> would cause the test to fail, but without providing a <a href="exceptions/StackDepth.html">stack depth</a> pointing
 * to the failing line of test code. This stack depth, provided by <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a> (and a
 * few other ScalaTest exceptions), makes it quicker for
 * users to navigate to the cause of the failure. Without <code>EitherValues</code>, to get
 * a stack depth exception you would need to make two statements, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Right(9)
 *
 * either should be ('left) // throws TestFailedException
 * either.left.get should be &gt; "Muchos problemas"
 * </pre>
 *
 * <p>
 * The <code>EitherValues</code> trait allows you to state that more concisely:
 * </p>
 *
 * <pre class="stHighlight">
 * val either: Either[String, Int] = Left("Muchas problemas")
 *
 * either.left.value should be &gt; 9 // either.left.value throws TestFailedException
 * </pre>
 */
trait EitherValues extends Serializable {

  // SKIP-DOTTY-START
  import scala.language.implicitConversions

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>LeftProjection</code>.
   *
   * @param leftProj the <code>LeftProjection</code> on which to add the <code>value</code> method
   */
  implicit def convertLeftProjectionToValuable[L, R](leftProj: Either.LeftProjection[L, R])(implicit pos: source.Position): LeftValuable[L, R] = new LeftValuable(leftProj, pos)

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>RightProjection</code>.
   *
   * @param rightProj the <code>RightProjection</code> on which to add the <code>value</code> method
   */
  @deprecated("The .right.value syntax on Either has been deprecated and will be removed in a future version of ScalaTest. Please use .value instead.", "3.2.3")
  implicit def convertRightProjectionToValuable[L, R](rightProj: Either.RightProjection[L, R])(implicit pos: source.Position): RightValuable[L, R] = new RightValuable(rightProj, pos)

  /**
   * Implicit conversion that adds a <code>value</code> method to <code>Either</code>.
   * This method is right biased and is the equivalent of calling <code>either.right.value</code>.
   *
   * @param either the <code>Either</code> on which to add the <code>value</code> method
   */
  implicit def convertEitherToValuable[L, R](either: Either[L, R])(implicit pos: source.Position): EitherValuable[L, R] = new EitherValuable(either, pos)
  // SKIP-DOTTY-END

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert <code>LeftProjection</code> to <code>LeftValuable</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param leftProj the <code>LeftProjection</code> on which to be converted to <code>LeftValuable</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertLeftProjectionToValuable[L, R](leftProj: Either.LeftProjection[L, R])(implicit pos: source.Position): LeftValuable[L, R] = new LeftValuable(leftProj, pos)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension methods for <code>LeftProjection</code> that add a <code>value</code> method.
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [L, R](leftProj: Either.LeftProjection[L, R])(using pos: source.Position) {  
  //DOTTY-ONLY   def value: L = 
  //DOTTY-ONLY     convertLeftProjectionToValuable(leftProj)(pos).value
  //DOTTY-ONLY }

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert <code>RightProjection</code> to <code>RightValuable</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param rightProj the <code>RightProjection</code> on which to be converted to <code>RightValuable</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY @deprecated("The .right.value syntax on Either has been deprecated and will be removed in a future version of ScalaTest. Please use .value instead.", "3.2.3")
  //DOTTY-ONLY def convertRightProjectionToValuable[L, R](rightProj: Either.RightProjection[L, R])(implicit pos: source.Position): RightValuable[L, R] = new RightValuable(rightProj, pos)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension methods for <code>RightProjection</code> that add a <code>value</code> method.
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [L, R](rightProj: Either.RightProjection[L, R])(using pos: source.Position) {
  //DOTTY-ONLY   @deprecated("The .right.value syntax on Either has been deprecated and will be removed in a future version of ScalaTest. Please use .value instead.", "3.2.3")
  //DOTTY-ONLY   def value: R = 
  //DOTTY-ONLY     (new RightValuable(rightProj, pos)).value
  //DOTTY-ONLY }

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Convert <code>Either</code> to <code>EitherValuable</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param either the <code>Either</code> on which to be converted to <code>EitherValuable</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertEitherToValuable[L, R](either: Either[L, R])(implicit pos: source.Position): EitherValuable[L, R] = new EitherValuable(either, pos)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Extension methods for <code>Either</code> that add a <code>value</code> method.
  //DOTTY-ONLY  * This method is right biased and is the equivalent of calling <code>either.right.value</code>.
  //DOTTY-ONLY  */
  //DOTTY-ONLY extension [L, R](either: Either[L, R])(using pos: source.Position) {  
  //DOTTY-ONLY   def value: R = 
  //DOTTY-ONLY     (new EitherValuable(either, pos)).value
  //DOTTY-ONLY }

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
  class LeftValuable[L, R](leftProj: Either.LeftProjection[L, R], pos: source.Position) extends Serializable {

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
          val e = leftProj.e
          val p = pos
          throw new TestFailedException((_: StackDepthException) => Some(Resources.eitherLeftValueNotDefined(e)), Some(cause), p)
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
  class RightValuable[L, R](rightProj: Either.RightProjection[L, R], pos: source.Position) extends Serializable {

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
          val e = rightProj.e
          val p = pos
          throw new TestFailedException((_: StackDepthException) => Some(Resources.eitherRightValueNotDefined(e)), Some(cause), p)
      }
    }
  }

  /**
   * Wrapper class that adds a <code>value</code> method to <code>Either</code>, allowing
   * you to make statements to inspect the value if a Right, like:
   *
   * <pre class="stHighlight">
   * either.value should be &gt; 9
   * </pre>
   *
   * @param either An <code>Either</code> to convert to <code>EitherValuable</code>, which provides the
   *   <code>value</code> method.
   */ 
  class EitherValuable[L, R](either: Either[L, R], pos: source.Position) extends Serializable {
    /**
     * Returns the <code>Right</code> value contained in the wrapped <code>RightProjection</code>, if defined as a <code>Right</code>, else throws <code>TestFailedException</code> with
     * a detail message indicating the <code>Either</code> was defined as a <code>Right</code>, not a <code>Left</code>.
     */
    def value: R = {
      either match {
        case Right(r) => r
        case _ =>
          val e = either
          val p = pos
          throw new TestFailedException((_: StackDepthException) => Some(Resources.eitherValueNotDefined(e)), None, p)
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
 * scala&gt; import matchers.Matchers._
 * import matchers.Matchers._
 * 
 * scala&gt; import EitherValues._
 * import EitherValues._
 * 
 * scala&gt; val e: Either[String, Int] = Left("Muchas problemas")
 * e: Either[String,Int] = Left(Muchos problemas)
 * 
 * scala&gt; e.left.value should be ("Muchos problemas")
 * 
 * scala&gt; e.value should be &lt; 9
 * org.scalatest.TestFailedException: The Either on which value was invoked was not defined.
 *   at org.scalatest.EitherValues$RightValuable.value(EitherValues.scala:148)
 *   at .&lt;init&gt;(&lt;console&gt;:18)
 *   ...
 * </pre>
 */
object EitherValues extends EitherValues
