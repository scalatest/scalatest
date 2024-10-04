/*
 * Copyright 2001-2024 Artima, Inc.
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

import enablers.Futuristic

/**
 * Trait that provides a <code>complete</code>-<code>lastly</code> construct, which ensures
 * cleanup code in <code>lastly</code> is executed whether the code passed to <code>complete</code>
 * completes abruptly with an exception or successfully results in a <code>Future</code>,
 * <a href="FutureOutcome.html"><code>FutureOutcome</code></a>, or other type with an
 * implicit <a href="enablers/Futuristic.html"><code>Futuristic</code></a> instance.
 *
 * <p>
 * This trait is mixed into ScalaTest's async testing styles, to make it easy to ensure
 * cleanup code will execute whether code that produces a "futuristic" value (any type <code>F</code>
 * for which a <code>Futuristic[F]</code> instance is implicitly available). ScalaTest provides
 * implicit <code>Futuristic</code> instances for <code>Future[T]</code> for any type <code>T</code>
 * and <code>FutureOutcome</code>.
 * </p>
 *
 * <p>
 * If the future-producing code passed to <code>complete</code> throws an
 * exception, the cleanup code passed to <code>lastly</code> will be executed immediately, and the same exception will
 * be rethrown, unless the code passed to <code>lastly</code> also completes abruptly with an exception. In that case,
 * <code>complete</code>-<code>lastly</code> will complete abruptly with the exception thrown by the code passed to
 * <code>lastly</code> (this mimics the behavior of <code>finally</code>).
 * </p>
 *
 * <p>
 * Otherwise, if the code passed to <code>complete</code> successfully returns a <code>Future</code> (or other "futuristic" type),
 * <code>complete</code>-<code>lastly</code>
 * will register the cleanup code to be performed once the future completes and return a new future that will complete
 * once the original future completes <em>and</em> the subsequent cleanup code has completed execution. The future returned by
 * <code>complete</code>-<code>lastly</code> will have the same result as the original future passed to <code>complete</code>,
 * unless the cleanup code throws an exception. If the cleanup code passed to <code>lastly</code> throws
 * an exception, the future returned by <code>lastly</code> will fail with that exception.
 * </p>
 *
 * <p>
 * The <code>complete</code>-<code>lastly</code> syntax is intended to be used to ensure cleanup code is executed
 * in async testing styles like <code>try</code>-<code>finally</code> is used in traditional testing styles.
 * Here's an example of <code>complete</code>-<code>lastly</code>
 * used in <code>withFixture</code> in an async testing style:
 * </p>
 *
 * <pre class="stHighlight">
 * // Your implementation
 * override def withFixture(test: NoArgAsyncTest) = {
 *
 *   // Perform setup here
 *
 *   complete {
 *     super.withFixture(test) // Invoke the test function
 *   } lastly {
 *     // Perform cleanup here
 *   }
 * }
 * </pre>
 */
trait CompleteLastly {

  /**
   * Class that provides the <code>lastly</code> method of the <code>complete</code>-<code>lastly</code> syntax.
   *
   * @param futuristicBlock a by-name that produces a futuristic type
   * @param futuristic the futuristic typeclass instance
   */
  class ResultOfCompleteInvocation[T](futuristicBlock: => T, futuristic: Futuristic[T]) {

   /**
    * Registers cleanup code to be executed immediately if the future-producing code passed
    * to <code>complete</code> throws an exception, or otherwise asynchronously, when the future
    * returned by the code passed to <code>complete</code> itself completes.
    *
    * <p>
    * See the main documentation for trait <code>CompleteLastly</code> for more detail.
    * </p>
    *
    * @param lastlyBlock cleanup code to execute whether the code passed to <code>complete</code>
    *           throws an exception or succesfully returns a futuristic value.
    */
    def lastly(lastlyBlock: => Unit): T = {
      val result: T =
        try futuristicBlock // evaluate the by-name once
        catch {
          case ex: Throwable =>
            lastlyBlock  // execute the clean up
            throw ex // rethrow the same exception
        }
      futuristic.withCleanup(result) { lastlyBlock }
    }

    /**
     * Pretty string representation of this class.
     */
    override def toString = "ResultOfCompleteInvocation"
  }

  /**
   * Registers a block of code that produces any "futuristic" type (any type <code>F</code> for which
   * an implicit <a href="enablers/Futuristic.html"><code>Futuristic[F]</code></a> instance is implicitly available), returning
   * an object that offers a <code>lastly</code> method. 
    *
    * <p>
    * See the main documentation for trait <code>CompleteLastly</code> for more detail.
    * </p>
    *
    * @param completeBlock cleanup code to execute whether the code passed to <code>complete</code>
    *           throws an exception or succesfully returns a futuristic value.
   */
  def complete[T](completeBlock: => T)(implicit futuristic: Futuristic[T]): ResultOfCompleteInvocation[T] =
    new ResultOfCompleteInvocation[T](completeBlock, futuristic)
}

/**
 * Companion object that facilitates the importing of <code>CompleteLastly</code> members as 
 * an alternative to mixing it in.
 */
object CompleteLastly extends CompleteLastly

