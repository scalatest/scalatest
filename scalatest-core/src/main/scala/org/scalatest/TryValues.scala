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
package org.scalatest

import java.util.NoSuchElementException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.exceptions.TestFailedException

import scala.util.Try
import scala.util.Failure
import scala.util.Success

/**
 * Trait that provides an implicit conversion that adds <code>success</code> and <code>failure</code> methods
 * to <code>scala.util.Try</code>, enabling you to make assertions about the value of a <code>Success</code> or
 * the exception of a <code>Failure</code>.
 *
 * <p>
 * The <code>success</code> method will return the <code>Try</code> on which it is invoked as a <code>Success</code> if the <code>Try</code>
 * actually is a <code>Success</code>, or throw <code>TestFailedException</code> if not.
 * The <code>failure</code> method will return the <code>Try</code> on which it is invoked as a <code>Failure</code> if the <code>Try</code>
 * actually is a <code>Failure</code>, or throw <code>TestFailedException</code> if not.
 * </p>
 *
 * <p>
 * This construct allows you to express in one statement that an <code>Try</code> should be either a <code>Success</code>
 * or a <code>Failure</code> and that its value or exception, respectively,should meet some expectation. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * try1.success.value should be &gt; 9
 * try2.failure.exception should have message "/ by zero"
 * </pre>
 *
 * <p>
 * Or, using assertions instead of a matchers:
 * </p>
 *
 * <pre class="stHighlight">
 * assert(try1.success.value &gt; 9)
 * assert(try2.failure.exception.getMessage == "/ by zero")
 * </pre>
 *
 * <p>
 * Were you to simply invoke <code>get</code> on the <code>Try</code>, 
 * if the <code>Try</code> wasn't a <code>Success</code>, it would throw the exception contained in the <code>Failure</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val try2 = Try { 1 / 0 }
 *
 * try2.get should be &lt; 9 // try2.get throws ArithmeticException
 * </pre>
 *
 * <p>
 * The <code>ArithmeticException</code> would cause the test to fail, but without providing a <a href="exceptions/StackDepth.html">stack depth</a> pointing
 * to the failing line of test code. This stack depth, provided by <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a> (and a
 * few other ScalaTest exceptions), makes it quicker for
 * users to navigate to the cause of the failure. Without <a href="TryValues.html"><code>TryValues</code></a>, to get
 * a stack depth exception you would need to make two statements, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * try2 should be a 'success // throws TestFailedException
 * try2.get should be &lt; 9
 * </pre>
 *
 * <p>
 * The <code>TryValues</code> trait allows you to state that more concisely:
 * </p>
 *
 * <pre class="stHighlight">
 * try2.success.value should be &lt; 9 // throws TestFailedException
 * </pre>
 *
 */
trait TryValues {

  import scala.language.implicitConversions

  /**
   * Implicit conversion that adds <code>success</code> and <code>failure</code> methods to <code>Try</code>.
   *
   * @param theTry the <code>Try</code> to which to add the <code>success</code> and <code>failure</code> methods
   */
  implicit def convertTryToSuccessOrFailure[T](theTry: Try[T]) = new SuccessOrFailure(theTry)

  /**
   * Wrapper class that adds <code>success</code> and <code>failure</code> methods to <code>scala.util.Try</code>, allowing
   * you to make statements like:
   *
   * <pre class="stHighlight">
   * try1.success.value should be &gt; 9
   * try2.failure.exception should have message "/ by zero"
   * </pre>
   *
   * @param theTry An <code>Try</code> to convert to <code>SuccessOrFailure</code>, which provides the <code>success</code> and <code>failure</code> methods.
   */
  class SuccessOrFailure[T](theTry: Try[T]) {

    /**
     * Returns the <code>Try</code> passed to the constructor as a <code>Failure</code>, if it is a <code>Failure</code>, else throws <code>TestFailedException</code> with
     * a detail message indicating the <code>Try</code> was not a <code>Failure</code>.
     */
    def failure: Failure[T] = {
      theTry match {
        case failure: Failure[T] => failure
        case _ => 
          throw new TestFailedException(sde => Some(Resources.tryNotAFailure), None, getStackDepthFun("TryValues.scala", "failure"))
      }
    }

    /**
     * Returns the <code>Try</code> passed to the constructor as a <code>Success</code>, if it is a <code>Success</code>, else throws <code>TestFailedException</code> with
     * a detail message indicating the <code>Try</code> was not a <code>Success</code>.
     */
    def success: Success[T] = {
      theTry match {
        case success: Success[T] => success
        case _ => 
          throw new TestFailedException(sde => Some(Resources.tryNotASuccess), None, getStackDepthFun("TryValues.scala", "success"))
      }
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>TryValues</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TryValues</code>'s members so you can use
 * <code>success</code> and <code>failure</code> on <code>Try</code> in the Scala interpreter.
 * </pre>
 */
object TryValues extends TryValues
